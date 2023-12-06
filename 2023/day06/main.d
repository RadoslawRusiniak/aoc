import core.bitop, std.bitmanip;
import core.checkedint;
import core.time;
import std.algorithm, std.functional, std.meta;
import std.array, std.container;
import std.bigint;
import std.conv;
import std.format;
import std.math, std.numeric;
import std.range, std.range.interfaces;
import std.stdio, std.string;
import std.ascii, std.typecons;
import std.datetime.date, std.datetime.systime;

alias ResultType = int;

alias Times = int[];
alias Distances = int[];
alias Race = Tuple!(int, "time", int, "distance");
alias State = Tuple!(Times, "times", Distances, "distances");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    auto res = solveEasy(input);
    // auto res = solveHard(state);

    res.writeln;
}

int[] parseArrayLine(string input) => input.split(":")[1].split.map!(to!int).array;

Times parseTimes(string input) => input.parseArrayLine;

Distances parseDistances(string input) => input.parseArrayLine;

State parseEasy(string[] input) {
    auto ts = input[0].parseTimes;
    auto ds = input[1].parseDistances;
    return State(ts, ds);
}

ResultType numberOfWaysToBeat(Race race) 
    =>
    iota(1, race.time)
    .count!(speed => speed * (race.time - speed) > race.distance);

Race[] toRaces(State state) => zip(state.times, state.distances).map!Race.array;

ResultType solveEasy(string[] input)
    =>
    input.parseEasy
    .toRaces
    .map!numberOfWaysToBeat
    .fold!((a, b) => a * b);

ResultType solveHard(State state) {
    return 0;
}