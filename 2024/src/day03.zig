const std = @import("std");
const print = std.debug.print;

const Error = error{
    ParseLineError,
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

    var total: i32 = 0;

    var buff: [64]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buff, ')')) |str| {
        total += try do_mult_instruction(str);
    }
    return total;
}

fn do_mult_instruction(str: []const u8) !i32 {
    var tokenizer = std.mem.tokenizeSequence(u8, str, "mul(");
    while (tokenizer.next()) |token| {
        var operands = std.mem.tokenizeSequence(u8, token, ",");
        var op1: i32 = 0;
        var op2: i32 = 0;
        if (operands.next()) |n| {
            if (n.len > 3 or n.len < 1) continue;
            op1 = std.fmt.parseInt(i32, n, 10) catch continue;
        } else {
            continue;
        }

        if (operands.next()) |n| {
            if (n.len > 3 or n.len < 1) continue;
            op2 = std.fmt.parseInt(i32, n, 10) catch continue;
        } else {
            continue;
        }

        // Too many operands
        if (operands.next()) |n| {
            _ = n;
            continue;
        } else {
            // If we make it here, we have two valid operands.
            return op1 * op2;
        }
    }
    // Could not find a valid mul instruction.
    return 0;
}

fn part2(infile: []const u8) !i32 {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };

    var total: i32 = 0;

    var buff: [64]u8 = undefined;
    var do_mult = true;
    while (try file.reader().readUntilDelimiterOrEof(&buff, ')')) |str| {
        if (std.mem.endsWith(u8, str, "don't(")) {
            do_mult = false;
        } else if (std.mem.endsWith(u8, str, "do(")) {
            do_mult = true;
        }

        if (do_mult) {
            total += try do_mult_instruction(str);
        }
    }
    return total;
}

const testing = std.testing;

test "Test input for part 1" {
    const input = "./inputs/day03/test.txt";
    const res = try part1(input);
    try testing.expect(res == 161);
}

test "Test input for part 2" {
    const input = "./inputs/day03/test2.txt";
    const res = try part2(input);
    print("Result: {d}\n", .{res});
    try testing.expect(res == 48);
}
