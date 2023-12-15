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

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

string[] parse(string[] input) => input[0].split(',');

int getHash(string input) {
    auto cur = 0;
    foreach (c; input) {
        cur += c.to!int;
        cur *= 17;
        cur %= 256;
    }

    return cur;
}

long solve(string[] input) => input.map!getHash.sum;

long solveEasy(string[] input) => input.parse.solve;

alias Order = Tuple!(string, "text", char, "op", int, "focalLen");
alias Lense = Tuple!(string, "text", int, "focalLen");

Order toOrder(string input) {
    auto text = input.until!(c => c == '=' || c == '-').to!string;
    char op = input[text.length];
    int focalLen;
    if (op == '=') { focalLen = input[text.length+1..$].to!int; }

    return Order(text, op, focalLen);
}

long solveHard(string[] input) {
    auto orders = input.parse.map!toOrder;

    debug { orders.writeln; }

    Lense[][] boxes = new Lense[][] (256);
    foreach (o; orders) {
        auto hash = getHash(o.text);
        if (o.op == '=') {
            auto pos = boxes[hash].countUntil!(lense => lense.text == o.text);
            if (pos == -1) { 
                boxes[hash] ~= Lense(o.text, o.focalLen); 
            } else { 
                boxes[hash][pos].focalLen = o.focalLen; 
            }
        } else {
            boxes[hash] = boxes[hash].remove!(lense => lense.text == o.text);
        }
    }

    debug { boxes[0 .. 5].each!writeln; }

    auto ans = 0;
    foreach (i, box; boxes.enumerate(1)) {
        foreach (j, e; box.enumerate(1)) {
            ans += i * j * e.focalLen;
        }
    }

    return ans;
}