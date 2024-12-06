const std = @import("std");
const print = std.debug.print;

const Error = error{
    InvalidLineError,
    FileNotFoundError,
};

pub fn solve(input_file: []const u8) !void {
    const p1 = try part1(input_file);
    print("Part 1: {d}\n", .{p1});
    const p2 = try part2(input_file);
    print("Part 2: {d}\n", .{p2});
}

fn part1(infile: []const u8) !i32 {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };

    var count_safe: i32 = 0;

    var buff: [64]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        var tokenizer = std.mem.tokenizeSequence(u8, line, " ");

        const first = tokenizer.next() orelse return Error.InvalidLineError;
        var prev = try std.fmt.parseInt(i32, first, 10);
        var inc: i32 = 0;

        var safe = true;
        while (tokenizer.next()) |token| {
            const num = try std.fmt.parseInt(i32, token, 10);
            const diff = num - prev;
            if (@abs(diff) < 1 or @abs(diff) > 3 or inc == 1 and diff < 0 or inc == -1 and diff > 0) {
                safe = false;
                break;
            }
            if (inc == 0) {
                inc = if (diff > 0) 1 else -1;
            }

            prev = num;
        }
        if (safe) {
            count_safe += 1;
        }
    }

    return count_safe;
}

fn part2(infile: []const u8) !i32 {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };

    var count_safe: i32 = 0;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var buff: [64]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        const safe = try is_safe(line, allocator);
        if (safe) {
            count_safe += 1;
        }
    }

    return count_safe;
}

fn is_safe(line: []u8, allocator: std.mem.Allocator) !bool {
    var tokenizer = std.mem.tokenizeSequence(u8, line, " ");
    const first = tokenizer.next() orelse return Error.InvalidLineError;
    const cur = try std.fmt.parseInt(i32, first, 10);
    const prev: ?i32 = null;
    // Try both increasing and decreasing.
    // Tracking if we are increasing/decreasing while processing is a headache.
    return try is_safe_rec(prev, cur, false, tokenizer.rest(), true, false, allocator) or try is_safe_rec(prev, cur, false, tokenizer.rest(), false, true, allocator);
}

fn is_safe_rec(prev: ?i32, cur: ?i32, level_removed: bool, rest: []const u8, inc: bool, dec: bool, allocator: std.mem.Allocator) !bool {
    if (cur == null) {
        return true;
    }
    var tokenizer = std.mem.tokenizeSequence(u8, rest, " ");
    var next: ?i32 = null;
    if (tokenizer.next()) |next_token| {
        next = try std.fmt.parseInt(i32, next_token, 10);
    }

    var invalid_prev = false;
    if (prev != null) {
        const diff_prev = cur.? - prev.?;
        invalid_prev = invalid_diff(diff_prev, inc, dec);
    }

    var invalid_cur = false;
    if (next != null) {
        const diff_cur = next.? - cur.?;
        invalid_cur = invalid_diff(diff_cur, inc, dec);
    }

    if (invalid_prev or invalid_cur) {
        if (level_removed) {
            return false;
        }

        const new_rest = tokenizer.rest();

        const new_rest_c = try allocator.alloc(u8, new_rest.len);
        std.mem.copyForwards(u8, new_rest_c, new_rest);
        defer allocator.free(new_rest_c);

        const new_rest_n = try allocator.alloc(u8, new_rest.len);
        defer allocator.free(new_rest_n);
        std.mem.copyForwards(u8, new_rest_n, new_rest);

        return (try is_safe_rec(prev, next, true, new_rest_c, inc, dec, allocator) or try is_safe_rec(prev, cur, true, new_rest_n, inc, dec, allocator));
    } else {
        return is_safe_rec(cur.?, next, level_removed, tokenizer.rest(), inc, dec, allocator);
    }
}

fn invalid_diff(diff: i32, inc: bool, dec: bool) bool {
    return @abs(diff) < 1 or @abs(diff) > 3 or (inc and diff < 0) or (dec and diff > 0);
}

const testing = std.testing;

test "Test input for part 1" {
    const input = "./inputs/day02/test.txt";
    const res = try part1(input);
    try testing.expect(res == 2);
}

test "Test input for part 2" {
    const input = "./inputs/day02/test.txt";
    const res = try part2(input);
    try testing.expect(res == 4);
}
