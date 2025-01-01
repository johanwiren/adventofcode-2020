const std = @import("std");
const utils = @import("utils.zig");
const fs = std.fs;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const day01 = @import("adventofcode_2024/day_01.zig");
const day02 = @import("adventofcode_2024/day_02.zig");

pub fn main() !void {
    try utils.run(allocator, 2024, 1, day01.part1solver, day01.part2solver);
    try utils.run(allocator, 2024, 2, day02.part1solver, day02.part2solver);
}
