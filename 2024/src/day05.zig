const std = @import("std");
const print = std.debug.print;

const Error = error{
    ParseLineError,
    FileNotFoundError,
    RuleCreationError,
};

pub fn solve(input_file: []const u8) !void {
    const p1 = try part1(input_file);
    print("Part 1: {d}\n", .{p1});
    // const p2 = try part2(input_file);
    // print("Part 2: {d}\n", .{p2});
}

fn part1(infile: []const u8) !i32 {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Use a map to store all the pages that should come after
    // a page.
    var pageRulesAfter = std.AutoHashMap(i32, std.ArrayList(i32)).init(allocator);
    defer pageRulesAfter.deinit();

    // Use a map to store all the pages that should come before
    // a page.
    var pageRulesBefore = std.AutoHashMap(i32, std.ArrayList(i32)).init(allocator);
    defer pageRulesBefore.deinit();

    const reader = file.reader();
    var buff: [1024]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        if (line.len == 0) {
            break;
        }
        var tokenizer = std.mem.tokenizeSequence(u8, line, "|");

        const n1 = tokenizer.next() orelse return Error.ParseLineError;
        const n2 = tokenizer.next() orelse return Error.ParseLineError;
        const before = try std.fmt.parseInt(i32, n1, 10);
        const after = try std.fmt.parseInt(i32, n2, 10);

        try insert_page(&pageRulesAfter, before, after, allocator);
        try insert_page(&pageRulesBefore, after, before, allocator);
    }

    var total: i32 = 0;
    // Process updates
    while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        // Check the pages that should come after,
        // reverse and check the pages that should come before
        const line_rev = try reverse_update(line, allocator);
        defer allocator.free(line_rev);

        if (try updates_in_order(line, pageRulesAfter, allocator) and
            try updates_in_order(line_rev, pageRulesBefore, allocator))
        {
            total += try get_mid_page(line);
        }
    }

    return total;
}

fn insert_page(map: *std.AutoHashMap(i32, std.ArrayList(i32)), key: i32, value: i32, allocator: std.mem.Allocator) !void {
    if (map.getPtr(key)) |pages| {
        pages.append(value) catch {
            return Error.RuleCreationError;
        };
    } else {
        var pages = std.ArrayList(i32).init(allocator);
        pages.append(value) catch {
            return Error.RuleCreationError;
        };
        try map.put(key, pages);
    }
}

fn get_mid_page(update: []const u8) !i32 {
    var it = std.mem.split(u8, update, ",");
    var len: i32 = 0;
    while (it.next()) |n| {
        _ = n;
        len += 1;
    }

    it.reset();

    var count: i32 = 0;
    while (it.next()) |n| {
        if (@divFloor(len, 2) == count) {
            return try std.fmt.parseInt(i32, n, 10);
        }
        count += 1;
    }
    return -1;
}

fn reverse_update(update: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var pages = std.ArrayList([]const u8).init(allocator);
    defer pages.deinit();

    var it = std.mem.split(u8, update, ",");
    while (it.next()) |number| {
        try pages.append(number);
    }

    var reversed = std.ArrayList([]const u8).init(allocator);
    defer reversed.deinit();
    const max_index = if (pages.items.len > 1) pages.items.len - 1 else 0;
    for (0..max_index) |i| {
        try reversed.append(pages.items[max_index - i]);
    }

    return std.mem.join(allocator, ",", reversed.items);
}

fn updates_in_order(update: []const u8, rules: std.AutoHashMap(i32, std.ArrayList(i32)), allocator: std.mem.Allocator) !bool {
    var tokenizer = std.mem.tokenizeSequence(u8, update, ",");
    var pages = std.ArrayList(i32).init(allocator);
    defer pages.deinit();
    while (tokenizer.next()) |page| {
        const page_num = try std.fmt.parseInt(i32, page, 10);
        if (rules.getPtr(page_num)) |after| {
            for (after.items) |a| {
                for (pages.items) |p| {
                    if (p == a) {
                        return false;
                    }
                }
            }
        }
        try pages.append(page_num);
    }
    return true;
}

const testing = std.testing;

test "Test input for part 1" {
    const input = "./inputs/day05/test.txt";
    const res = try part1(input);
    try testing.expect(res == 143);
}

// test "Test input for part 2" {
//     const input = "./inputs/day05/test.txt";
//     const res = try part2(input);
//     try testing.expect(res == 9);
// }
