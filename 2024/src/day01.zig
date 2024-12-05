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

const Lists = std.meta.Tuple(&.{[]i32, []i32});
fn parse_input(infile: []const u8, allocator: std.mem.Allocator) !Lists {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };

    var list1 = std.ArrayList(i32).init(allocator);
    defer list1.deinit();
    var list2 = std.ArrayList(i32).init(allocator);
    defer list2.deinit();

    var buff: [64]u8 = undefined;
    while(try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        var tokenizer = std.mem.tokenizeSequence(u8, line, " ");
        var first_int: i32 = 0;
        var second_int: i32 = 0;
        const first = tokenizer.next() orelse return Error.ParseLineError;
        const second = tokenizer.next() orelse return Error.ParseLineError;
        first_int = try std.fmt.parseInt(i32, first, 10);
        second_int = try std.fmt.parseInt(i32, second, 10);
        try list1.append(first_int);
        try list2.append(second_int);
    }

    const l1 = try list1.toOwnedSlice();
    const l2 = try list2.toOwnedSlice();
    return .{l1, l2};
}

fn part1(infile: []const u8 ) !u32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const l1, const l2 = try parse_input(infile, allocator);
    defer allocator.free(l1);
    defer allocator.free(l2);

    std.mem.sort(i32, l1, {}, std.sort.asc(i32));
    std.mem.sort(i32, l2, {}, std.sort.asc(i32));

    var diff: u32 = 0;
    for(l1, 0..) |item, index| {
        diff += @abs(item - l2[index]);
    }

    return diff;
}

fn part2(infile: []const u8 ) !i32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();


    const l1, const l2 = try parse_input(infile, allocator);
    defer allocator.free(l1);
    defer allocator.free(l2);

    var freqMap = std.AutoHashMap(i32, i32).init(allocator);
    defer freqMap.deinit();

    for(l2) |item| {
        try freqMap.put(item, (freqMap.get(item) orelse 0) + 1);
    }

    var total: i32 = 0;
    for (l1) |item| {
        total += item * (freqMap.get(item) orelse 0);
    }

    return total;
}

const testing = std.testing;

test "Test input for part 1" {
    const input = "./inputs/day01/test.txt";
    const res = try part1(input);
    try testing.expect(res == 11);
}

test "Test input for part 2" {
    const input = "./inputs/day01/test.txt";
    const res = try part2(input);
    print("Result: {d}\n", .{res});
    try testing.expect(res == 31);
}
