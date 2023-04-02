const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const assert = std.debug.assert;

const quiver = @import("quiver.zig");
const Quiver = quiver.Quiver;

const prelude = @import("prelude.zig");
const print1 = prelude.print1;
const allocator = prelude.allocator;
const global_random = prelude.global_random;
const Str = prelude.Str;
const V = prelude.V;
const C = prelude.C;

pub const Destroyed = std.math.maxInt(V);

pub const HEID = usize;
pub const HENotPresent = std.math.maxInt(HEID);

pub const Slot = u4;
pub const Move = struct { slot: Slot, card: C };
//pub const LayerFilterType = enum { wildcard, fixed, equal, move };
pub const LayerFilter = union(enum) {
    wildcard: void,
    fixed: V,
    equal: Slot,
    move: Move,
};

const HyperedgeFmt = enum(u8) { binary = 'b', decimal = 'd', alphabet = 'a', any = 'z' };

//pub const LayerTransformType = enum { fixed, copy, move };
pub const LayerTransform = union(enum) {
    fixed: V,
    copy: Slot,
    move: Move,
};

pub fn Bundle(comptime numLayers: u8) type {
    return struct {
        const Layers = [numLayers]Quiver;

        const Hyperedge = [numLayers]V;
        const HyperedgeList = ArrayList(Hyperedge);

        layers: Layers,
        numLayerVertices: [numLayers]V,
        productVertices: HEID,
        strides: [numLayers]usize,

        hyperedgeIndex: []HEID,
        hyperedges: HyperedgeList,
        numHyperedges: HEID,
        alphabet: Str, // optional, but useful

        const Self = @This();

        pub fn init(layers: Layers) Self {
            var self: Self = undefined;

            self.layers = layers;
            self.productVertices = 1;
            self.hyperedges = HyperedgeList.initCapacity(allocator, 512) catch unreachable;
            self.numHyperedges = 0;

            var stride: usize = 1;
            for (0..numLayers) |j| {
                const i = numLayers - j - 1;
                const num = layers[i].numVertices;
                self.productVertices *= num;
                self.strides[i] = stride;
                self.numLayerVertices[i] = num;
                stride *= num;
            }

            self.hyperedgeIndex = allocator.alloc(HEID, self.productVertices) catch unreachable;
            std.mem.set(HEID, self.hyperedgeIndex, HENotPresent);

            return self;
        }

        pub fn getHyperedge(self: *const Self, id: HEID) Hyperedge {
            return self.hyperedges.items[id];
        }

        fn destroyHyperedge(self: *Self, hyperedge: Hyperedge) void {
            const id = self.hyperedgeID(hyperedge);
            var index = self.hyperedgeIndex[id];
            if (index != HENotPresent) {
                self.hyperedgeIndex[id] = HENotPresent;
                self.hyperedges.items[index][0] = Destroyed;
            }
        }

        pub fn getHyperedgeIndex(self: *Self, hyperedge: Hyperedge) HEID {
            const id = self.hyperedgeID(hyperedge);
            var index = self.hyperedgeIndex[id];
            if (index == HENotPresent) {
                index = self.numHyperedges;
                self.hyperedgeIndex[id] = index;
                self.hyperedges.append(hyperedge) catch unreachable;
                self.numHyperedges += 1;
            }
            return index;
        }

        pub fn hyperedgeID(self: *Self, hyperedge: Hyperedge) HEID {
            var number: HEID = 0;
            for (hyperedge, self.strides) |v, stride| {
                number += @intCast(usize, v) * @intCast(usize, stride);
            }
            return number;
        }

        pub fn writeHyperedge(self: *const Self, edge: Hyperedge, fmt: HyperedgeFmt, writer: anytype) void {
            switch (fmt) {
                .binary => writer.writeAll(edge),
                .decimal => for (edge) |v| writer.writeInt(v),
                .alphabet => for (edge, 0..) |v, i| writer.writeByte(self.layers[i].inAlphabet(v)),
                .any => writer.format("{}", edge),
            }
        }

        pub fn format(self: *const Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = options;
            var edgeFmt = .any;
            if (fmt.len) edgeFmt = @intToEnum(HyperedgeFmt, fmt[0]);

            if (edgeFmt == .any) try writer.writeAll("Bundle{ ");
            for (self.hyperedges.items) |edge| self.writeHyperedge(edge, edgeFmt);
            if (edgeFmt == .any) try writer.writeAll(" }");
        }

        pub fn asString(self: *const Self, baseIndex: usize, alphabetIndex: usize) Str {
            var len = self.layers[baseIndex].numVertices - 1;
            var buf = allocator.alloc(u8, len) catch unreachable;
            var alphabetLayer = self.layers[alphabetIndex];
            for (0..len) |i| buf[i] = ' ';
            for (self.hyperedges.items) |edge| {
                const v0 = edge[baseIndex];
                const v1 = edge[alphabetIndex];
                var char: u8 = '.';
                if (v0 == Destroyed) continue;
                if (v1 > 0) char = alphabetLayer.inAlphabet(v1);
                buf[v0 - 1] = char;
            }
            return buf;
        }

        pub fn dumpAsString(self: *const Self) void {
            print("StringBundle(\"{s}\")\n", .{self.asString(0, 1)});
        }

        const LHSMatch = []Hyperedge;

        const Filter = [numLayers]LayerFilter;

        pub fn testFilter(self: *const Self, filter: Filter, candidate: Hyperedge, match: LHSMatch) bool {
            for (candidate, filter, 0..) |vertex, layerFilter, i| {
                const matched = switch (layerFilter) {
                    .wildcard => true,
                    .fixed => |filterVertex| (vertex == filterVertex),
                    .equal => |filterSlot| (vertex == match[filterSlot][i]),
                    .move => |moveData| (vertex == self.layers[i].applyCardinal(match[moveData.slot][i], moveData.card)),
                };
                if (!matched) return false;
            }
            return true;
        }

        const MatchStep = struct {
            target: Slot,
            filter: Filter,

            pub fn execute(self: *const MatchStep, bundle: *const Self, candidate: Hyperedge, match: LHSMatch) bool {
                if (candidate[0] == Destroyed) return false;
                if (!bundle.testFilter(self.filter, candidate, match)) return false;
                match[self.target] = candidate;
                return true;
            }
        };

        const LHS = struct { numSlots: usize, tape: []MatchStep };

        pub fn parseLHS(self: *Self, str: Str) LHS {
            const numSlots = std.mem.indexOfScalar(u8, str, ';').?;
            var tape = allocator.alloc(MatchStep, numSlots) catch unreachable;
            assert(std.mem.count(u8, str, ";") == numLayers - 1);
            var l: usize = 0;
            var it = std.mem.split(u8, str, ";");
            while (it.next()) |spec| {
                for (spec, 0..) |char, i| {
                    tape[i].target = @intCast(Slot, i);
                    tape[i].filter[l] = switch (char) {
                        '*' => .wildcard,
                        ';' => break,
                        '+' => .{ .move = .{ .slot = @intCast(Slot, i - 1), .card = 0 } },
                        else => .{ .fixed = self.layers[l].fromAlphabet(char) },
                    };
                }
                l += 1;
            }
            return .{ .numSlots = numSlots, .tape = tape };
        }

        pub fn parseStringLHS(self: *Self, str: Str) LHS {
            const numSlots = str.len;
            var tape = allocator.alloc(MatchStep, numSlots) catch unreachable;
            var alphabetLayer = self.layers[1];
            for (str, 0..) |char, i| {
                var slot = @intCast(Slot, i);
                tape[i].target = slot;
                tape[i].filter[0] = (if (i == 0) .wildcard else .{ .move = .{ .slot = slot - 1, .card = 0 } });
                tape[i].filter[1] = switch (char) {
                    '*' => .wildcard,
                    ';' => break,
                    '+' => .{ .move = .{ .slot = slot - 1, .card = 0 } },
                    else => .{ .fixed = alphabetLayer.fromAlphabet(char) },
                };
            }
            return .{ .numSlots = numSlots, .tape = tape };
        }

        const MatchIterator = struct {
            tape: []MatchStep,
            bundle: *const Self,
            partialMatch: LHSMatch,
            stack: [2]u16,
            depth: u8,

            // this implements a non-recursive way of finding all matches. it maintains a user stack between calls
            // (which should be made obselete when async functions return to Zig)
            pub fn next(self: *MatchIterator) ?LHSMatch {
                const bundle = self.bundle;
                const edges = bundle.hyperedges.items;
                const match = self.partialMatch;
                const max_i = edges.len;
                const tape = self.tape;
                const max_d = tape.len - 1;
                var stack = self.stack;
                var d = self.depth;
                defer {
                    //print("done, stack = {d}\n", .{stack});
                    self.depth = d; // make sure we save depth on return
                    self.stack = stack;
                }
                while (true) {
                    //print("  stack = {d}, step = {d}\n", .{ stack, d });
                    // if we bumped past the end, 'carry the 1'
                    while (stack[d] == max_i) {
                        stack[d] = 0;
                        if (d == 0) return null;
                        d -= 1;
                        //print("  exhausted, step back to {d}\n", .{d});
                        stack[d] += 1;
                    }
                    // try match the rest of the tape
                    while (d <= max_d) : (d += 1) {
                        //print("  trying edge {d} for step {d}\n", .{ edges[stack[d]], d });
                        if (!tape[d].execute(bundle, edges[stack[d]], match)) break;
                        //print("  test passed, now on step {d}\n", .{d + 1});
                    } else {
                        // if we got to the end of tape, bump index and return the match
                        d -= 1;
                        stack[d] += 1;
                        //print("  found match for {d}, back to step {d}\n", .{ match, d });
                        return match;
                    }
                    // otherwise, we failed, so bump index to try the next one
                    stack[d] += 1;
                }
            }
        };

        pub fn matches(self: *const Self, lhs: LHS) MatchIterator {
            return .{
                .tape = lhs.tape,
                .bundle = self,
                .partialMatch = allocator.alloc(Hyperedge, lhs.numSlots) catch unreachable,
                .stack = std.mem.zeroes([2]u16),
                .depth = 0,
            };
        }

        pub fn countMatches(self: *const Self, lhs: LHS) usize {
            var iter = self.matches(lhs);
            var count: usize = 0;
            while (iter.next() != null) count += 1;
            return count;
        }

        // find all matching hyperedges to the elements of the filter stack
        pub fn findFirstMatch(self: *const Self, lhs: LHS) ?LHSMatch {
            var iter = self.matches(lhs);
            return iter.next();
        }

        pub fn findNthMatch(self: *const Self, lhs: LHS, n: usize) ?LHSMatch {
            var iter = self.matches(lhs);
            for (0..n) |_| _ = iter.next();
            return iter.next();
        }

        // find all matching hyperedges to the elements of the filter stack
        pub fn findAllMatches(self: *const Self, lhs: LHS) []LHSMatch {
            var iter = self.matches(lhs);
            var matchList = ArrayList(LHSMatch).init(allocator);
            while (iter.next()) |item| {
                var copy = allocator.dupe(Hyperedge, item) catch unreachable;
                matchList.append(copy) catch unreachable;
            }
            return matchList.items;
        }

        const RHSMatch = []Hyperedge;

        const Transform = [numLayers]LayerTransform;

        pub fn computeTransform(self: *const Self, transform: Transform, match: LHSMatch) Hyperedge {
            var output: Hyperedge = undefined;
            for (transform, 0..) |layerTransform, i| {
                output[i] = switch (layerTransform) {
                    .fixed => |vertex| vertex,
                    .copy => |slot| match[slot][i],
                    .move => |moveData| self.layers[i].applyCardinal(match[moveData.slot][i], moveData.card),
                };
            }
            return output;
        }

        const RHS = []Transform;

        pub fn parseRHS(self: *Self, str: Str) RHS {
            const numSlots = std.mem.indexOfScalar(u8, str, ';').?;
            var trans = allocator.alloc(Transform, numSlots) catch unreachable;
            assert(std.mem.count(u8, str, ";") == numLayers - 1);
            var l: usize = 0;
            var it = std.mem.split(u8, str, ";");
            while (it.next()) |spec| {
                for (spec, 0..) |char, i| {
                    trans[i][l] = switch (char) {
                        '=' => .{ .copy = @intCast(Slot, i - 1) },
                        '+' => .{ .move = .{ .slot = @intCast(Slot, i - 1), .card = 0 } },
                        'A'...'D' => .{ .copy = @intCast(Slot, std.mem.indexOfScalar(u8, "ABCD", char).?) },
                        else => .{ .fixed = self.layers[l].fromAlphabet(char) },
                    };
                }
                l += 1;
            }
            return trans;
        }

        pub fn parseStringRHS(self: *Self, str: Str) RHS {
            const numSlots = str.len;
            var lhs = allocator.alloc(Transform, numSlots) catch unreachable;
            var alphabetLayer = self.layers[1];
            for (str, lhs, 0..) |char, *trans, i| {
                trans[0] = .{ .copy = @intCast(Slot, i) };
                trans[1] = switch (char) {
                    '=' => .{ .copy = @intCast(Slot, i - 1) },
                    '+' => .{ .move = .{ .slot = @intCast(Slot, i - 1), .card = 0 } },
                    'A'...'D' => .{ .copy = @intCast(Slot, std.mem.indexOfScalar(u8, "ABCD", char).?) },
                    else => .{ .fixed = alphabetLayer.fromAlphabet(char) },
                };
            }
            return lhs;
        }

        pub const RewriteRule = struct {
            lhs: LHS,
            rhs: RHS,

            pub fn computeOutputs(rule: *RewriteRule, bundle: *Self, inputs: LHSMatch) RHSMatch {
                var outputs = allocator.alloc(Hyperedge, rule.rhs.len) catch unreachable;
                for (0.., rule.rhs) |i, transform| {
                    outputs[i] = bundle.computeTransform(transform, inputs);
                }
                return outputs;
            }

            pub fn findFirst(rule: *RewriteRule, bundle: *Self) ?Rewrite {
                var inputs = bundle.findFirstMatch(rule.lhs) orelse return null;
                var outputs = rule.computeOutputs(bundle, inputs);
                return .{ .inputs = inputs, .outputs = outputs };
            }

            pub fn findRandom(rule: *RewriteRule, bundle: *Self, random: std.rand.Random) ?Rewrite {
                var allRewrites = rule.findAll(bundle);
                if (allRewrites.len == 0) return null;
                const i = random.uintAtMost(usize, allRewrites.len - 1);
                return allRewrites[i];
            }

            pub fn findNth(rule: *RewriteRule, bundle: *Self, n: usize) ?Rewrite {
                var inputs = bundle.findNthMatch(rule.lhs, n) orelse return null;
                var outputs = rule.computeOutputs(bundle, inputs);
                return .{ .inputs = inputs, .outputs = outputs };
            }

            pub fn findAll(rule: *RewriteRule, bundle: *Self) []Rewrite {
                var list = ArrayList(Rewrite).init(allocator);
                var iter = bundle.matches(rule.lhs);
                while (iter.next()) |match| {
                    var inputs = allocator.dupe(Hyperedge, match) catch unreachable;
                    var outputs = rule.computeOutputs(bundle, inputs);
                    list.append(.{ .inputs = inputs, .outputs = outputs }) catch unreachable;
                }
                return list.items;
            }

            pub fn executeFirst(rule: *RewriteRule, bundle: *Self) bool {
                var rewrite = rule.findFirst(bundle) orelse return false;
                rewrite.apply(bundle);
                return true;
            }

            pub fn executeRandom(rule: *RewriteRule, bundle: *Self, random: std.rand.Random) bool {
                var rewrite = rule.findRandom(bundle, random) orelse return false;
                rewrite.apply(bundle);
                return true;
            }

            pub fn executeNth(rule: *RewriteRule, bundle: *Self, n: usize) bool {
                var rewrite = rule.findNth(bundle, n) orelse return false;
                rewrite.apply(bundle);
                return true;
            }
        };

        pub fn parseRule(self: *Self, lhs: Str, rhs: Str) RewriteRule {
            return .{ .lhs = self.parseLHS(lhs), .rhs = self.parseRHS(rhs) };
        }

        pub fn parseStringRule(self: *Self, lhs: Str, rhs: Str) RewriteRule {
            return .{
                .lhs = self.parseStringLHS(lhs),
                .rhs = self.parseStringRHS(rhs),
            };
        }

        const Ruleset = []RewriteRule;

        pub fn executeRuleset(bundle: *Self, ruleset: Ruleset) bool {
            for (ruleset) |*rule| {
                if (rule.executeFirst(bundle)) return true;
            }
            return false;
        }

        pub fn executeRulesetRandom(bundle: *Self, ruleset: Ruleset, random: std.rand.Random) bool {
            var rule_counts = allocator.alloc(usize, ruleset.len) catch unreachable;
            std.mem.set(usize, rule_counts, 0);
            defer allocator.free(rule_counts);
            var total_count: usize = 0;
            for (ruleset, rule_counts) |*rule, *count| {
                const c = bundle.countMatches(rule.lhs);
                total_count += c;
                count.* += c;
            }
            if (total_count == 0) return false;
            const rule_index = random.weightedIndex(usize, rule_counts);
            var rule = ruleset[rule_index];
            const match_index = random.uintAtMost(usize, rule_counts[rule_index] - 1);
            return rule.executeNth(bundle, match_index);
        }

        const Rewrite = struct {
            inputs: LHSMatch,
            outputs: RHSMatch,

            pub fn format(self: *const Rewrite, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = fmt;
                _ = options;
                try writer.print("{d} => {d}", .{ self.inputs, self.outputs });
            }

            pub fn apply(self: *const Rewrite, bundle: *Self) void {
                for (self.inputs) |input| _ = bundle.destroyHyperedge(input);
                for (self.outputs) |output| _ = bundle.getHyperedgeIndex(output);
            }

            pub fn applyNonDestructively(self: *const Rewrite, bundle: *Self) void {
                for (self.outputs) |output| _ = bundle.getHyperedgeIndex(output);
            }
        };
    };
}

pub fn StringBundle(alphabet: Str, initial: Str) Bundle(2) {
    var baseQuiver = Quiver.line(@intCast(u8, initial.len));
    var alphabetQuiver = Quiver.alphabet(alphabet);
    var bundle = Bundle(2).init(.{ baseQuiver, alphabetQuiver });

    for (1.., initial) |pos, char| {
        const charIndex = std.mem.indexOfScalar(u8, alphabet, char).?;
        _ = bundle.getHyperedgeIndex(.{ @intCast(u8, pos), @intCast(u8, charIndex + 1) });
    }

    return bundle;
}

const expectFmt = std.testing.expectFmt;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expect = std.testing.expect;

test "findMatches" {
    var bundle = StringBundle("()", "(()()");
    var lhs = bundle.parseLHS("*+;()");
    var all = bundle.findAllMatches(lhs);
    try expectFmt("{ { { 2, 1 }, { 3, 2 } }, { { 4, 1 }, { 5, 2 } } }", "{d}", .{all});
    var first = bundle.findFirstMatch(lhs).?;
    try expectFmt("{ { 2, 1 }, { 3, 2 } }", "{d}", .{first});
}

const move0: LayerFilter = .{ .move = .{ .slot = 0, .card = 0 } };
const move1: LayerFilter = .{ .move = .{ .slot = 1, .card = 0 } };
const copy0: LayerTransform = .{ .copy = 0 };
const copy1: LayerTransform = .{ .copy = 1 };

test "findRewrites" {
    var bundle = StringBundle("()", "(()()");
    const MatchStepT = @TypeOf(bundle).MatchStep;
    const LhsT = @TypeOf(bundle).LHS;
    const TransformT = @TypeOf(bundle).Transform;
    const RewriteRuleT = @TypeOf(bundle).RewriteRule;

    var tape: [2]MatchStepT = .{
        .{ .target = 0, .filter = .{ .wildcard, .{ .fixed = 1 } } },
        .{ .target = 1, .filter = .{ move0, .{ .fixed = 2 } } },
    };
    var lhs: LhsT = .{ .numSlots = 2, .tape = tape[0..tape.len] };
    var rhs: [1]TransformT = .{.{ copy0, copy1 }};
    var rule: RewriteRuleT = .{ .lhs = lhs, .rhs = &rhs };
    var first = rule.findFirst(&bundle).?;
    try expectFmt("{ { 2, 1 }, { 3, 2 } } => { { 2, 2 } }", "{d}", .{first});

    rhs = .{.{ copy1, copy0 }};
    rule = .{ .lhs = lhs, .rhs = &rhs };
    first = rule.findFirst(&bundle).?;
    try expectFmt("{ { 2, 1 }, { 3, 2 } } => { { 3, 1 } }", "{d}", .{first});
}

test "applyRewrites" {
    var bundle = StringBundle("()", "(()()");
    var rule = bundle.parseStringRule("()", "");

    try expectEqualStrings("(()()", bundle.asString(0, 1));

    try expect(rule.executeFirst(&bundle));
    try expectEqualStrings("(  ()", bundle.asString(0, 1));

    try expect(rule.executeFirst(&bundle));
    try expectEqualStrings("(    ", bundle.asString(0, 1));

    try expect(!rule.executeFirst(&bundle));
    try expectEqualStrings("(    ", bundle.asString(0, 1));

    bundle = StringBundle("()", "(()()");
    rule = bundle.parseStringRule("()", "BA");

    try expect(rule.executeFirst(&bundle));
    try expectEqualStrings("()(()", bundle.asString(0, 1));

    try expect(rule.executeFirst(&bundle));
    try expectEqualStrings(")((()", bundle.asString(0, 1));

    try expect(rule.executeFirst(&bundle));
    try expectEqualStrings(")(()(", bundle.asString(0, 1));
}

test "applyNth" {
    var bundle = StringBundle("ab", "aabbb");
    var rule1 = bundle.parseStringRule("a", ".");
    var rule2 = bundle.parseStringRule("b", ".");

    try expect(rule1.executeNth(&bundle, 1));
    try expectEqualStrings("a.bbb", bundle.asString(0, 1));

    try expect(rule2.executeNth(&bundle, 2));
    try expectEqualStrings("a.bb.", bundle.asString(0, 1));

    try expect(rule2.executeNth(&bundle, 1));
    try expectEqualStrings("a.b..", bundle.asString(0, 1));

    try expect(rule1.executeNth(&bundle, 0));
    try expectEqualStrings("..b..", bundle.asString(0, 1));

    try expect(!rule1.executeNth(&bundle, 0));
    try expectEqualStrings("..b..", bundle.asString(0, 1));

    try expect(rule2.executeNth(&bundle, 0));
    try expectEqualStrings(".....", bundle.asString(0, 1));

    try expect(!rule2.executeNth(&bundle, 0));
    try expectEqualStrings(".....", bundle.asString(0, 1));
}

test "applyRandom" {
    var bundle = StringBundle("ab", "aaaaaabbbbbb");
    var rule1 = bundle.parseStringRule("a", ".");
    var rule2 = bundle.parseStringRule("b", ".");

    var prng = std.rand.DefaultPrng.init(0);
    var random = prng.random();

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings("a.aaaabbbbbb", bundle.asString(0, 1));

    try expect(rule2.executeRandom(&bundle, random));
    try expectEqualStrings("a.aaaabb.bbb", bundle.asString(0, 1));

    try expect(rule2.executeRandom(&bundle, random));
    try expectEqualStrings("a.aaaab..bbb", bundle.asString(0, 1));

    try expect(rule2.executeRandom(&bundle, random));
    try expectEqualStrings("a.aaaa...bbb", bundle.asString(0, 1));

    try expect(rule2.executeRandom(&bundle, random));
    try expectEqualStrings("a.aaaa...b.b", bundle.asString(0, 1));

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings("..aaaa...b.b", bundle.asString(0, 1));

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings("..aaa....b.b", bundle.asString(0, 1));

    bundle = StringBundle("ab", "aaaaaaaaaaaa");
    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings("aaaaaaaaaa.a", bundle.asString(0, 1));

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings("aaa.aaaaaa.a", bundle.asString(0, 1));

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings(".aa.aaaaaa.a", bundle.asString(0, 1));

    try expect(rule1.executeRandom(&bundle, random));
    try expectEqualStrings(".aa..aaaaa.a", bundle.asString(0, 1));
}

test "applyRandomRuleset" {
    var bundle = StringBundle("ab", "aaaaaaaaaabbbbbbbbbbbbbb");

    var rule1 = bundle.parseStringRule("a", ".");
    var rule2 = bundle.parseStringRule("b", ".");

    var rules: [2]Bundle(2).RewriteRule = .{ rule1, rule2 };
    var ruleset = rules[0..];

    var prng = std.rand.DefaultPrng.init(0);
    var random = prng.random();
    prng.seed(99);
    try expectEqual(bundle.countMatches(rule1.lhs), 10);
    try expectEqual(bundle.countMatches(rule2.lhs), 14);

    try expect(bundle.executeRulesetRandom(ruleset, random));
    try expectEqualStrings("aaaaaaa.aabbbbbbbbbbbbbb", bundle.asString(0, 1));

    try expect(bundle.executeRulesetRandom(ruleset, random));
    try expectEqualStrings("aaaaaaa.aa.bbbbbbbbbbbbb", bundle.asString(0, 1));

    try expect(bundle.executeRulesetRandom(ruleset, random));
    try expectEqualStrings("aaaaaaa.aa.bbbbbb.bbbbbb", bundle.asString(0, 1));

    var i: usize = 0;
    while (bundle.executeRulesetRandom(ruleset, random)) {
        i += 1;
    }

    try expectEqual(i, 21);
    try expectEqualStrings("........................", bundle.asString(0, 1));
}
