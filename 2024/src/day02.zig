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
    while(try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        var tokenizer = std.mem.tokenizeSequence(u8, line, " ");

        const first = tokenizer.next() orelse return Error.InvalidLineError;
        var prev = try std.fmt.parseInt(i32, first, 10);
        var inc: i32 = 0;

        var safe = true;
        while (tokenizer.next()) |token| {
            const num = try std.fmt.parseInt(i32, token, 10);
            const diff = num - prev;
            if (@abs(diff) < 1 
                or @abs(diff) > 3
                or inc == 1 and diff < 0
                or inc == -1 and diff > 0
                ) {
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

    var buff: [64]u8 = undefined;
    while(try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        const bad_levels = try count_bad_levels(line);
        if (bad_levels < 2 ) {
            count_safe += 1;
        }
    }

    return count_safe;
}

fn count_bad_levels(line: []u8) !i32 {
    var tokenizer = std.mem.tokenizeSequence(u8, line, " ");

    const first = tokenizer.next() orelse return Error.InvalidLineError;
    var prev = try std.fmt.parseInt(i32, first, 10);
    var inc: i32 = 0;

    // TODO: Handle edge cases
    // - The first level being bad
    // - Not skipping the right level?
    var bad_levels: i32 = 0;
    while (tokenizer.next()) |token| {
        const cur = try std.fmt.parseInt(i32, token, 10);
        const diff = cur - prev;
        if (@abs(diff) < 1 
            or @abs(diff) > 3
            or inc == 1 and diff < 0
            or inc == -1 and diff > 0
            ) {
            bad_levels += 1;
            continue;
        }
        if (inc == 0) {
            inc = if (diff > 0) 1 else -1;
        }

        prev = cur;
    }
    return bad_levels;
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
