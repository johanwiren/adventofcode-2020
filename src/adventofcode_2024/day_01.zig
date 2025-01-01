const utils = @import("utils.zig");
const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn parse(input: []u8) ![][]i64 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var result = try std.ArrayList([]i64).initCapacity(allocator, 1000);
    defer result.deinit();
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeAny(u8, line, " ");
        var row = try std.ArrayList(i64).initCapacity(allocator, 2);
        while (tokens.next()) |token| {
            const value = try std.fmt.parseInt(i64, token, 10);
            try row.append(value);
        }
        try result.append(row.items);
    }
    return result.toOwnedSlice();
}

pub fn part1solver(input: []u8) ![]u8 {
    const parsed = try parse(input);
    const transposed = try utils.transpose(allocator, i64, parsed);
    for (transposed) |xs| {
        std.mem.sort(i64, xs, {}, std.sort.asc(i64));
    }
    var sum: u64 = 0;
    for (transposed[0], transposed[1]) |a, b| {
        sum += @abs(a - b);
    }
    return std.fmt.allocPrint(allocator, "{d}", .{sum});
}

pub fn part2solver(input: []u8) ![]u8 {
    const parsed = try parse(input);
    const transposed = try utils.transpose(allocator, i64, parsed);
    var freqs = std.AutoHashMap(i64, i64).init(allocator);
    defer freqs.deinit();
    for (transposed[1]) |x| {
        try freqs.put(x, (freqs.get(x) orelse 0) + 1);
    }
    var sum: i64 = 0;
    for (transposed[0]) |x| {
        sum += (freqs.get(x) orelse 0) * x;
    }
    return std.fmt.allocPrint(allocator, "{d}", .{sum});
}

pub fn main() !void {
    try utils.run(allocator, 2024, 1, part1solver, part2solver);
}
