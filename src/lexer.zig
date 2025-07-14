const std = @import("std");

pub const TokenType = enum {
    none,id,num,string,eof,keyword_true,keyword_false,
    lparen,rparen,comma,assign,period,concat,
    func,end,ret,keyword_if,keyword_else,keyword_elseif,then,keyword_while,keyword_do,
    add,sub,mul,div,
    equals,nequals,bang,lt,gt,
    print,
};

pub const Token = struct {
    kind: TokenType,
    value: []const u8,
    line: usize,

    pub fn init(kind:TokenType,value:[]const u8,line:usize) Token {
        return .{.kind=kind,.value=value,.line=line};
    }

    pub fn is_operator(self:*Token) bool {
        return (self.kind == .add or self.kind == .sub or self.kind == .mul or self.kind == .div or self.kind == .equals
                or self.kind == .nequals or self.kind == .bang or self.kind == .concat
                or self.kind == .lt or self.kind == .gt);
    }
};

pub const Lexer = struct {
    tokens: std.ArrayList(Token),
    i: usize = 0,
    line: usize = 1,
    code: []const u8,
    c: u8,

    pub fn init(code:[]const u8) Lexer {
        return .{
            .code = code,
            .tokens = std.ArrayList(Token).init(std.heap.page_allocator),
            .c = code[0]
        };
    }

    pub fn deinit(self:*Lexer) void {
        self.tokens.deinit();
    }

    fn advance(self:*Lexer) u8 {
        self.i += 1;
        const c = self.c;
        self.c = self.code[self.i];
        return c;
    }

    fn collect_id(self:*Lexer) Token {
        var tk_type:TokenType = .id;
        const begin = self.i;
        tk_type = .id;
        while (std.ascii.isAlphanumeric(self.c) or self.c == '_') _ = self.advance();
        const end = self.i;
        const line = self.line;
        const slice = self.code[begin..end];

        if (std.mem.eql(u8,slice,"func")) tk_type = .func;
        if (std.mem.eql(u8,slice,"end")) tk_type = .end;
        if (std.mem.eql(u8,slice,"return")) tk_type = .ret;

        if (std.mem.eql(u8,slice,"if")) tk_type = .keyword_if;
        if (std.mem.eql(u8,slice,"then")) tk_type = .then;
        if (std.mem.eql(u8,slice,"else")) tk_type = .keyword_else;
        if (std.mem.eql(u8,slice,"elseif")) tk_type = .keyword_elseif;

        if (std.mem.eql(u8,slice,"while")) tk_type = .keyword_while;
        if (std.mem.eql(u8,slice,"do")) tk_type = .keyword_do;

        if (std.mem.eql(u8,slice,"true")) tk_type = .keyword_true;
        if (std.mem.eql(u8,slice,"false")) tk_type = .keyword_false;

        if (std.mem.eql(u8,slice,"print")) tk_type = .print;

        return Token.init(tk_type,slice,line);
    }
    
    fn collect_num(self:*Lexer) Token {
        const begin = self.i;
        while (std.ascii.isDigit(self.c)) _ = self.advance();
        const end = self.i;
        const line = self.line;
        const slice = self.code[begin..end];
        return Token.init(.num,slice,line);
    }

    pub fn print_tokens(self:*Lexer) void {
        for (self.tokens.items) |token| {
            std.debug.print("TOKEN: {any} {s}\n",.{token.kind,token.value});
        }
    }

    fn collect_string(self:*Lexer) Token {
        const begin = self.i;
        _ = self.advance();
        while (self.c != '"') {
            const c = self.advance();
            if (std.ascii.isWhitespace(c) and c != ' ') {
                std.debug.print("ERROR: in line {d}, unterminated string\n",.{self.line});
                std.process.exit(0);
            }
        }

        const end = self.i;
        const line = self.line;
        const slice = self.code[begin..end];
        
        _ = self.advance();

        return Token.init(.string,slice,line);
    }

    pub fn skip_comment(self:*Lexer) void {
        while (self.c != '\n') {
            _ = self.advance();   
        }
    }


    pub fn tokenize(self:*Lexer) void {
        while (self.c != 0) {
            if (std.ascii.isWhitespace(self.c)) {
                if (self.advance() == '\n') self.line += 1;
                continue;
            }
            if (std.ascii.isAlphabetic(self.c)) {
                self.tokens.append(self.collect_id()) catch unreachable;
                continue;
            }
            if (std.ascii.isDigit(self.c)) {
                self.tokens.append(self.collect_num()) catch unreachable;
                continue;
            }
            const c = self.advance();
            switch (c) {
                '<' => {self.tokens.append(Token.init(.lt,"<",self.line)) catch unreachable;continue;},
                '>' => {self.tokens.append(Token.init(.gt,">",self.line)) catch unreachable;continue;},

                '(' => {self.tokens.append(Token.init(.lparen,"(",self.line)) catch unreachable;continue;},
                ')' => {self.tokens.append(Token.init(.rparen,")",self.line)) catch unreachable;continue;},
                ',' => {self.tokens.append(Token.init(.comma,",",self.line)) catch unreachable;continue;},
                '+' => {self.tokens.append(Token.init(.add,"+",self.line)) catch unreachable;continue;},
                '-' => {self.tokens.append(Token.init(.sub,"-",self.line)) catch unreachable;continue;},
                '*' => {self.tokens.append(Token.init(.mul,"*",self.line)) catch unreachable;continue;},
                '/' => {self.tokens.append(Token.init(.div,"/",self.line)) catch unreachable;continue;},
                '"' => {self.tokens.append(self.collect_string()) catch unreachable; continue;},
                '=' => {
                    if (self.c == '=') {
                        self.tokens.append(Token.init(.equals,"==",self.line)) catch unreachable;
                        _ = self.advance();
                        continue;
                    } else {
                        self.tokens.append(Token.init(.assign,"=",self.line)) catch unreachable;
                        continue;
                    }
                },
                '.' => {
                    if (self.c == '.') {
                        self.tokens.append(Token.init(.concat,"..",self.line)) catch unreachable;
                        _ = self.advance();
                        continue;
                    } else {
                        self.tokens.append(Token.init(.period,".",self.line)) catch unreachable;
                        continue;
                    }
                },
                '!' => {
                    if (self.c == '=') {
                        self.tokens.append(Token.init(.nequals,"!=",self.line)) catch unreachable;
                        _ = self.advance();
                        continue;
                    } else {
                        self.tokens.append(Token.init(.bang,"!",self.line)) catch unreachable;
                        continue;
                    }
                },
                '#' => {self.skip_comment(); continue;},
                else => {
                    std.debug.print("ERROR: in line {d}, undefined symbol {c}\n",.{self.line,c});
                    std.process.exit(0);
                }
            }
        }
        self.tokens.append(Token.init(.eof,"EOF",self.line)) catch unreachable;
    }
};