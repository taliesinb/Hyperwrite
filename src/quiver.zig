const std = @import("std");

const prelude = @import("prelude.zig");

const allocator = prelude.allocator;
const Str = prelude.Str;
const V = prelude.V;
const C = prelude.C;

pub const Quiver = struct {
    numVertices: V,
    numCardinals: C,
    cardVert2Vert: []V,
    alphabet: ?Str,

    const Self = Quiver;

    pub fn init(numVertices: V, numCardinals: C) Quiver {
        const realNumVertices = numVertices + 1;
        var cvv = allocator.alloc(V, realNumVertices * numCardinals) catch unreachable;
        std.mem.set(V, cvv, 0);
        return .{ .numVertices = realNumVertices, .numCardinals = numCardinals, .cardVert2Vert = cvv, .alphabet = null };
    }

    pub fn initEdgeList(numVertices: V, edgeList: Str) Quiver {
        const numCardinals = std.mem.count(u8, edgeList, ";") + 1;
        var cq = Quiver.init(numVertices, @intCast(V, numCardinals));
        var byNewline = std.mem.tokenize(u8, edgeList, ";");
        var card: C = 0;
        while (byNewline.next()) |line2| {
            var bySpace = std.mem.tokenize(u8, line2, " ");
            while (bySpace.next()) |edgeSeq| {
                var byArrow = std.mem.tokenize(u8, edgeSeq, "→");
                var m_source: ?V = null;
                while (byArrow.next()) |vert| {
                    const target = std.fmt.parseInt(V, vert, 0) catch unreachable;
                    if (target > numVertices)
                        std.debug.panic("specified vertex {d} exceeds {d}", .{ target, numVertices });
                    if (m_source) |source| {
                        cq.addEdge(card, source, target);
                    }
                    m_source = target;
                }
            }
            card += 1;
        }

        return cq;
    }

    pub fn discrete(numVertices: V) Self {
        return Quiver.init(numVertices, 0);
    }

    pub fn alphabet(str: Str) Self {
        var self = Quiver.init(@intCast(u8, str.len), 0);
        self.alphabet = str;
        return self;
    }

    pub fn line(numVertices: V) Self {
        var self = Quiver.init(numVertices, 1);
        for (1..numVertices) |i| {
            self.cardVert2Vert[i] = @intCast(V, i + 1);
        }
        self.alphabet = "123456789";
        return self;
    }

    pub fn circle(numVertices: V) Self {
        var self = Quiver.line(numVertices);
        self.cardVert2Vert[numVertices] = 1;
        return self;
    }

    pub fn toCardVertIndex(self: *const Self, card: u8, vert: V) usize {
        return (card * self.numVertices) + vert;
    }

    pub fn addEdge(self: *Self, card: u8, source: V, target: V) void {
        self.cardVert2Vert[self.toCardVertIndex(card, source)] = target;
    }

    pub fn applyCardinal(self: *const Self, source: V, card: u8) V {
        std.debug.assert(card < self.numCardinals);
        return self.cardVert2Vert[self.toCardVertIndex(card, source)];
    }

    pub fn inAlphabet(self: *const Self, vertex: V) u8 {
        if (vertex == 0) return '.';
        if (self.alphabet == null) return "123456789"[vertex];
        return self.alphabet.?[vertex - 1];
    }

    pub fn fromAlphabet(self: *const Self, char: u8) V {
        if (char == '.') return 0;
        if (self.alphabet == null) {
            if (char >= '1' and char <= '9') return char - '0';
            @panic("no alphabet");
        }
        const v = std.mem.indexOfScalar(u8, self.alphabet.?, char) orelse std.debug.panic("not in alphabet: {c}", .{char});
        return @intCast(u8, v) + 1;
    }
};

const expectFmt = std.testing.expectFmt;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const expect = std.testing.expect;

test "init" {
    var q = Quiver.initEdgeList(5, "1→2→3 4→5");
    try expectEqual(q.numVertices, 6); // because null vertex
    try expectEqual(q.numCardinals, 1);
    try expectEqual(q.applyCardinal(0, 0), 0);
    try expectEqual(q.applyCardinal(1, 0), 2);
    try expectEqual(q.applyCardinal(2, 0), 3);
    try expectEqual(q.applyCardinal(3, 0), 0);
    try expectEqual(q.applyCardinal(4, 0), 5);
    try expectEqual(q.applyCardinal(5, 0), 0);

    q = Quiver.initEdgeList(3, "1→2;1→3→1");
    try expectEqual(q.numVertices, 4);
    try expectEqual(q.numCardinals, 2);
    try expectEqual(q.applyCardinal(1, 0), 2);
    try expectEqual(q.applyCardinal(2, 0), 0);
    try expectEqual(q.applyCardinal(1, 1), 3);
    try expectEqual(q.applyCardinal(2, 1), 0);
    try expectEqual(q.applyCardinal(3, 1), 1);

    q = Quiver.discrete(3);
    try expectEqual(q.numVertices, 4);
    try expectEqual(q.numCardinals, 0);

    q = Quiver.line(10);
    try expectEqual(q.numVertices, 11);
    try expectEqual(q.numCardinals, 1);
    for (1..10) |i| {
        var j = @intCast(V, i);
        try expectEqual(q.applyCardinal(j, 0), j + 1);
    }
    try expectEqual(q.applyCardinal(10, 0), 0);

    q = Quiver.circle(10);
    try expectEqual(q.numVertices, 11);
    try expectEqual(q.numCardinals, 1);
    for (1..10) |i| {
        var j = @intCast(V, i);
        try expectEqual(q.applyCardinal(j, 0), j + 1);
    }
    try expectEqual(q.applyCardinal(10, 0), 1);
}

test "alphabet" {
    var q = Quiver.line(5);
    try expectEqual(q.inAlphabet(0), '.');
    try expectEqual(q.inAlphabet(1), '1');
    try expectEqual(q.inAlphabet(5), '5');
    try expectEqual(q.fromAlphabet('.'), 0);
    try expectEqual(q.fromAlphabet('1'), 1);
    try expectEqual(q.fromAlphabet('5'), 5);

    q = Quiver.alphabet("abc");
    try expectEqual(q.inAlphabet(0), '.');
    try expectEqual(q.inAlphabet(1), 'a');
    try expectEqual(q.inAlphabet(2), 'b');
    try expectEqual(q.inAlphabet(3), 'c');
    try expectEqual(q.fromAlphabet('.'), 0);
    try expectEqual(q.fromAlphabet('a'), 1);
    try expectEqual(q.fromAlphabet('b'), 2);
    try expectEqual(q.fromAlphabet('c'), 3);
}
