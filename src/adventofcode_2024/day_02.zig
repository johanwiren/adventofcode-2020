const utils = @import("utils.zig");
const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn parse(input: []u8) ![][]i32 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var result = try std.ArrayList([]i32).initCapacity(allocator, 1000);
    defer result.deinit();
    while (lines.next()) |line| {
        var tokens = std.mem.tokenizeAny(u8, line, " ");
        var row = try std.ArrayList(i32).initCapacity(allocator, 8);
        while (tokens.next()) |token| {
            const value = try std.fmt.parseInt(i32, token, 10);
            try row.append(value);
        }
        try result.append(row.items);
    }
    return result.toOwnedSlice();
}

fn every(comptime T: type, pred: fn (anytype) bool, array: []T) bool {
    for (array) |item| {
        if (!pred(item)) {
            return false;
        }
    }
    return true;
}

fn everyPred(fns: []fn (anytype) bool) fn (anytype) bool {
    return struct {
        fn inner(x: anytype) bool {
            for (fns) |f| {
                if (!f(x)) {
                    return false;
                }
            }
            return true;
        }
    }.inner;
}

fn isPos(x: anytype) bool {
    return x > 0;
}

fn isNeg(x: anytype) bool {
    return x < 0;
}

test "isPos" {
    try std.testing.expect(isPos(0.2));
    try std.testing.expect(isPos(-0.2) == false);
}

fn complement(f: fn (anytype) bool) fn (anytype) bool {
    return struct {
        fn inner(x: anytype) bool {
            return !f(x);
        }
    }.inner;
}

test "every" {
    var list = [_]i32{ 1, 2 };
    try std.testing.expect(every(i32, isPos, &list));
    list = [_]i32{ 1, -2 };
    try std.testing.expect(every(i32, isPos, &list) == false);
    list = [_]i32{ -1, -2 };
    try std.testing.expect(every(i32, complement(isPos), &list));
}

fn okDelta(delta: anytype) bool {
    return @abs(delta) <= 3;
}

fn isSafe(report: []i32) !bool {
    var deltas = try std.ArrayList(i32).initCapacity(allocator, report.len - 1);
    var pairs = std.mem.window(i32, report, 2, 1);
    while (pairs.next()) |pair| {
        try deltas.append(pair[0] - pair[1]);
    }
    return every(i32, okDelta, deltas.items) and
        (every(i32, isPos, deltas.items) or every(i32, isNeg, deltas.items));
}

pub fn part1solver(input: []u8) ![]u8 {
    const parsed = try parse(input);
    var safeCount: u32 = 0;
    for (parsed) |report| {
        if (try isSafe(report)) {
            safeCount += 1;
        }
    }
    return std.fmt.allocPrint(allocator, "{d}", .{safeCount});
}

pub fn part2solver(input: []u8) ![]u8 {
    const parsed = try parse(input);
    var safeCount: u32 = 0;
    for (parsed) |report| {
        for (0..report.len) |removeIdx| {
            var lenientReport = try std.ArrayList(i32).initCapacity(allocator, report.len - 1);
            for (0..report.len) |i| {
                if (i != removeIdx) {
                    try lenientReport.append(report[i]);
                }
            }
            if (try isSafe(lenientReport.items)) {
                safeCount += 1;
                break;
            }
        }
    }
    return std.fmt.allocPrint(allocator, "{d}", .{safeCount});
}

pub fn main() !void {
    try utils.run(allocator, 2024, 2, part1solver, part2solver);
}
