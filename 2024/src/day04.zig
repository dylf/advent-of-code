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

const Part = enum { one, two };
fn part1(infile: []const u8) !i32 {
    const res = try parse_input(infile, Part.one);
    return res;
}

fn part2(infile: []const u8) !i32 {
    const res = try parse_input(infile, Part.two);
    return res;
}

fn parse_input(infile: []const u8, part: Part) !i32 {
    const file = std.fs.cwd().openFile(infile, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return Error.FileNotFoundError;
    };
    var aa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer aa.deinit();
    const allocator = aa.allocator();

    var buff: [1024]u8 = undefined;

    var line_length: usize = 0;
    var num_lines: usize = 0;
    while (try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        if (line_length == 0) {
            line_length = line.len;
        } else if (line.len != line_length) {
            std.log.err("Line length mismatch: {d} != {d}", .{ line.len, line_length });
            return Error.ParseLineError;
        }
        num_lines += 1;
    }

    // Allocate the grid.
    const grid = try allocator.alloc([]u8, num_lines);

    // Read the file into the grid
    try file.seekTo(0);
    var i: usize = 0;
    while (try file.reader().readUntilDelimiterOrEof(&buff, '\n')) |line| {
        grid[i] = try allocator.alloc(u8, line.len);
        std.mem.copyForwards(u8, grid[i], line);
        i += 1;
    }

    switch (part) {
        Part.one => {
            return try find_xmas(grid);
        },
        Part.two => {
            return try find_cross(grid);
        },
    }
}

fn find_xmas(grid: [][]u8) !i32 {
    var count: i32 = 0;
    const Dir = std.meta.Tuple(&.{ i32, i32 });
    const dirs = [8]Dir{
        .{ 1, 0 },
        .{ 1, 1 },
        .{ 0, 1 },
        .{ -1, 1 },
        .{ -1, 0 },
        .{ -1, -1 },
        .{ 0, -1 },
        .{ 1, -1 },
    };
    for (grid, 0..) |row, x| {
        for (row, 0..) |col, y| {
            if (col == 'X') {
                for (dirs) |dir| {
                    if (find_letters_rec('X', "MAS", grid, x, y, dir[0], dir[1])) {
                        count += 1;
                    }
                }
            }
        }
    }
    return count;
}

fn find_letters_rec(cur_letter: u8, rest: []const u8, grid: [][]u8, x: usize, y: usize, dir_x: i32, dir_y: i32) bool {
    const max_col = grid.len;
    const max_row = grid[0].len;
    if (x >= max_row or y >= max_col) {
        return false;
    }
    if (grid[x][y] != cur_letter) {
        return false;
    }
    if (cur_letter == 'S') {
        return true;
    }
    // indexes are unsigned check if we can proceed.
    const xx: i32 = @intCast(x);
    const yy: i32 = @intCast(y);
    if (xx + dir_x < 0 or yy + dir_y < 0) {
        return false;
    }

    const new_x: usize = @intCast(xx + dir_x);
    const new_y: usize = @intCast(yy + dir_y);
    return find_letters_rec(rest[0], rest[1..], grid, new_x, new_y, dir_x, dir_y);
}

fn find_cross(grid: [][]u8) !i32 {
    var count: i32 = 0;
    // Don't scan outer edges to avoid out of bounds, they aren't valid anyways.
    const max_rows = (grid.len - 1);
    const max_cols = (grid[0].len - 1);
    for (1..max_rows) |x| {
        for (1..max_cols) |y| {
            if (grid[x][y] == 'A') {
                if ((grid[prev_index(x)][prev_index(y)] == 'M' and grid[x + 1][y + 1] == 'S' or
                    grid[prev_index(x)][prev_index(y)] == 'S' and grid[x + 1][y + 1] == 'M') and
                    (grid[prev_index(x)][y + 1] == 'M' and grid[x + 1][prev_index(y)] == 'S' or
                    grid[prev_index(x)][y + 1] == 'S' and grid[x + 1][prev_index(y)] == 'M'))
                {
                    count += 1;
                }
            }
        }
    }
    return count;
}

fn prev_index(x: usize) usize {
    const xx: i32 = @intCast(x);
    const new = xx - 1;
    return @intCast(new);
}

const testing = std.testing;

test "Test input for part 1" {
    const input = "./inputs/day04/test.txt";
    const res = try part1(input);
    try testing.expect(res == 18);
}

test "Test input for part 2" {
    const input = "./inputs/day04/test.txt";
    const res = try part2(input);
    try testing.expect(res == 9);
}
