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

alias ResultType = long;

alias Card = int;
alias Hand = Card[];
alias Bid = int;
alias HandWithBid = Tuple!(Hand, "hand", Bid, "bid");
alias State = HandWithBid[];

alias HandType = int;

alias HandStrength = Tuple!(HandType, Hand);

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    auto res = solveEasy(input);
    // auto res = solveHard(input);

    res.writeln;
}

Card parseCard(dchar c) {
    if (c.isDigit) { return c - '0'; }

    auto cards = ['T': 10, 'J': 11, 'Q': 12, 'K': 13, 'A': 14];

    return cards[c.to!char];
}

Hand parseHand(string input) => input.map!parseCard.array;

HandWithBid parseLine(string input) {
    Hand h; Bid b;
    auto splitted = input.split;
    auto hand = parseHand(splitted[0]);
    auto bid = splitted[1].to!int;
    return HandWithBid(hand, bid);
}

State parse(string[] input) => input.map!parseLine.array;

HandType getHandType(Hand h) {
    auto grouped = h.dup.sort.group.save;

    if (grouped.count == 1) { return 6; }
    
    if (grouped.count == 2) {
        if (grouped.any!(t => t[1] == 4)) { return 5; }
        
        return 4;
    }

    if (grouped.count == 3) {
        if (grouped.any!(t => t[1] == 3)) { return 3; }

        return 2;
    }

    if (grouped.count == 4) {
        return 1;
    }

    return 0;
}

HandStrength getHandStrength(Hand h) => tuple(h.getHandType, h);

ResultType solveEasy(string[] input)
    =>
    input
    .parse
    .schwartzSort!(hwb => hwb.hand.getHandStrength)
    .enumerate(1)
    .map!(t => t[0].to!long * t[1].bid)
    .sum(0L);

ResultType solveHard(string[] input) {
    return 0;
}