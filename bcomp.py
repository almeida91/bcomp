from pyparsing import *


class Function(object):
    def __init__(self, name):
        self.name = name
        self.statements = []


class Code(object):
    global_vars = []
    functions = []

    current_block = None

    def add_function(self, x):
        print x


class Parser(object):
    def __init__(self):
        identifier = Word(alphas + "_", alphanums + "_")
        integer = Word(nums)
        char = Literal("'").suppress() + Word(alphanums, exact=1) + Literal("'").suppress()
        constant = (char | integer)
        lpar = Literal('(').suppress()
        rpar = Literal(')').suppress()
        plusop = oneOf('+ -')
        multop = oneOf('/ * %')
        logical_binary_operators = oneOf('& | ^')
        inc_op = Word('+', exact=2) | Word('-', exact=2)

        expression = Forward()
        statement = Forward()
        statement_list = Forward()

        arguments = delimitedList(expression('exp'))
        function_call = ((identifier('name') + FollowedBy('(')) + lpar + Optional(arguments)('args') + rpar)

        precendence = [
            (multop, 2, opAssoc.LEFT),
            (plusop, 2, opAssoc.LEFT),
            (logical_binary_operators, 2, opAssoc.LEFT)
        ]
        expression << (operatorPrecedence(function_call | constant | identifier, precendence))
        logical_expression = operatorPrecedence(expression,
                                                [(logical_binary_operators, 2, opAssoc.LEFT)])
        assignment = (identifier + Word('=') + expression('value')).setParseAction(self.assignment)

        # statements
        return_statement = Keyword('return') + expression + Suppress(';')
        external_statement = Keyword('extrn') + identifier + Suppress(';')

        auto_atom = identifier + Optional(constant)
        declaration_statement = Keyword('auto') + auto_atom + ZeroOrMore(
            Literal(',') + auto_atom) + Suppress(';')
        assignment_statement = assignment + Suppress(';')
        function_call_statement = function_call + Suppress(';')

        if_statement = Keyword('if').setParseAction(self.begin_if) + (
            Suppress('(') + (assignment | expression) + Suppress(')')) + Empty().setParseAction(self.if_block) + (
            statement | (Suppress('{') + statement_list + Suppress('}'))) + Empty().setParseAction(
            self.finish_block) + Optional(
            Keyword('else') + (statement | (Suppress('{') + statement_list + Suppress('}')))) \
                           .setParseAction(self.finish_block)

        while_statement = Keyword('while').setParseAction(self.begin_while) + Suppress('(') + (
            assignment | expression) + Suppress(')') + Empty().setParseAction(self.while_block) + (
                              statement | Suppress('{') + statement_list + Suppress('}')) + Empty().setParseAction(
            self.finish_block)

        statement << (
            return_statement |
            external_statement |
            declaration_statement |
            assignment_statement |
            function_call_statement |
            if_statement |
            while_statement
        )
        statement_list << OneOrMore(statement)

        parameter_list = delimitedList(identifier)
        function_body = Suppress('{') + statement_list + Suppress('}')
        function = Empty().setParseAction(self.begin_function) + (identifier('name') + Group(
            Suppress('(') + Optional(parameter_list)('params') + Suppress(
                ')'))) + function_body + Empty().setParseAction(self.finish_block)

        global_declaration = identifier + constant + Suppress(';')

        self.program = OneOrMore(function | global_declaration)

        print

    def assignment(self, value):
        pass

    def begin_if(self, value):
        pass

    def if_block(self, value):
        pass

    def begin_while(self, value):
        pass

    def while_block(self, value):
        pass

    def else_block(self, value):
        pass

    def begin_function(self, value):
        pass

    def finish_block(self, value):
        pass

    def parse_string(self, string):
        return self.program.ignore(cStyleComment).parseString(string)


if __name__ == '__main__':
    parser = Parser()

    program = """
    /* The following function will print a non-negative number, n, to
  the base b, where 2<=b<=10,  This routine uses the fact that
  in the ANSCII character set, the digits O to 9 have sequential
  code values.  */

    printn(n,b) {
        extrn putchar;
        auto a 12, b;

        if(a=n/b) { /* assignment, not test for equality */
            printn(a, b); /* recursive */
        }
        putchar(n%b + '0');

        while(a) {
            a = b;
        }
}
    """

    print parser.parse_string(program)
