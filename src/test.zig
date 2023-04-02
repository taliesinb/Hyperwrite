pub const std = @import("std");
//pub const Allocator = std.mem.Allocator;
pub const expect = std.testing.expect;
pub const expectFmt = std.testing.expectFmt;

// test "foo" {
//     var r = std.rand.DefaultPrng.init(0);
//     for (1..20) |_| {
//         std.debug.print("{d}\n", std.rand.Random(r, u16, 0, 12));
//     }
// }

test {
    var lhs: [2]u8 = .{ 9, 9 };
    var lhs2: [4]u8 = .{ 0, 0, 0, 0 };
    std.mem.copy(u8, lhs2[2..], &lhs);
    std.debug.print("{d}\n", .{lhs2});
}
