const std = @import("std");
const fs = std.fs;

pub fn transpose(allocator: std.mem.Allocator, comptime T: type, array: [][]T) ![][]T {
    const rows = array.len;
    const cols = if (rows > 0) array[0].len else 0;
    var result = try std.ArrayList([]T).initCapacity(allocator, cols);
    for (0..cols) |j| {
        var col = try std.ArrayList(T).initCapacity(allocator, rows);
        for (0..rows) |i| {
            try col.append(array[i][j]);
        }
        try result.append(col.items);
    }
    return result.toOwnedSlice();
}

fn getInput(allocator: std.mem.Allocator, year: u32, day: u32) ![]u8 {
    const home = try std.process.getEnvVarOwned(allocator, "HOME");
    defer allocator.free(home);
    const input_file = try std.fmt.allocPrint(allocator, "{s}/.cache/aoc/{d}{d}", .{ home, year, day });
    defer allocator.free(input_file);
    const f = try fs.openFileAbsolute(input_file, .{ .mode = fs.File.OpenMode.read_only });
    const input_bytes = try f.reader().readAllAlloc(allocator, 1 << 20);
    defer f.close();
    return input_bytes;
}

pub fn run(allocator: std.mem.Allocator, year: u32, day: u32, comptime p1: fn (input: []u8) anyerror![]u8, comptime p2: fn (input: []u8) anyerror![]u8) !void {
    std.debug.print("== Day {d} ==\n", .{day});
    const input = try getInput(allocator, year, day);
    var start = std.time.microTimestamp();
    const part_1_result = try p1(input);
    var stop = std.time.microTimestamp();
    std.debug.print("Part 1: {s}, {d} us\n", .{ part_1_result, stop - start });

    start = std.time.microTimestamp();
    const part_2_result = try p2(input);
    stop = std.time.microTimestamp();
    std.debug.print("Part 2: {s}, {d} us\n", .{ part_2_result, stop - start });
}
