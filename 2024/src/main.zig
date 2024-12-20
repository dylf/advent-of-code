const std = @import("std");

const day01 = @import("day01.zig");
const day02 = @import("day02.zig");
const day03 = @import("day03.zig");
const day04 = @import("day04.zig");
const day05 = @import("day05.zig");
const Problems = enum { day01, day02, day03, day04, day05 };

const Error = error{
    MissingArgument,
    UnknownProblem,
};

pub fn main() !void {
    var args = std.process.args();
    var counter: usize = 0;
    var arg1: ?[]const u8 = null;
    var arg2: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (counter == 1) {
            arg1 = arg;
        }
        if (counter == 2) {
            arg2 = arg;
        }
        counter += 1;
    }

    const problem_name = arg1 orelse {
        std.log.err("Please supply the problem name as the first argument\n", .{});
        return Error.MissingArgument;
    };

    const input_file = arg2 orelse {
        std.log.err("Please supply the input file as the second argument\n", .{});
        return Error.MissingArgument;
    };

    const problem = std.meta.stringToEnum(Problems, problem_name) orelse {
        std.log.err("Unknown problem name: {s}\n", .{problem_name});
        return Error.UnknownProblem;
    };
    switch (problem) {
        Problems.day01 => {
            try day01.solve(input_file);
        },
        Problems.day02 => {
            try day02.solve(input_file);
        },
        Problems.day03 => {
            try day03.solve(input_file);
        },
        Problems.day04 => {
            try day04.solve(input_file);
        },
        Problems.day05 => {
            try day05.solve(input_file);
        },
    }
}
