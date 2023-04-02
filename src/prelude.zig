pub const std = @import("std");
pub const Allocator = std.mem.Allocator;
pub const expect = std.testing.expect;
pub const expectFmt = std.testing.expectFmt;

var prng = std.rand.DefaultPrng.init(0);
pub const global_random = prng.random();

pub var buffer: [65535]u8 = undefined;
pub var fba = std.heap.FixedBufferAllocator.init(&buffer);
pub const allocator = fba.allocator();

pub const V = u8;
pub const C = u8;

pub const Str = []const u8;

pub fn print1(arg: anytype) void {
    const T = @TypeOf(arg);
    const spec = switch (@typeInfo(T)) {
        .Pointer => "{d}\n",
        else => "{}\n",
    };
    std.debug.print(spec, .{arg});
}
