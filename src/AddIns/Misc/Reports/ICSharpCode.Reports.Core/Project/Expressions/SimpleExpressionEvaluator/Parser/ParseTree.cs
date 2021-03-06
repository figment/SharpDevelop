// Generated by TinyPG v1.2 available at www.codeproject.com

using System;
using System.Collections.Generic;
using System.Text;

namespace SimpleExpressionEvaluator.Parser
{
    #region ParseTree
    public class ParseErrors : List<ParseError>
    {
    }

    public class ParseError
    {
        private string message;
        private int code;
        private int line;
        private int col;
        private int pos;
        private int length;

        public int Code { get { return code; } }
        public int Line { get { return line; } }
        public int Column { get { return col; } }
        public int Position { get { return pos; } }
        public int Length { get { return length; } }
        public string Message { get { return message; } }

        public ParseError(string message, int code, ParseNode node) : this(message, code,  0, node.Token.StartPos, node.Token.StartPos, node.Token.Length)
        {
        }

        public ParseError(string message, int code, int line, int col, int pos, int length)
        {
            this.message = message;
            this.code = code;
            this.line = line;
            this.col = col;
            this.pos = pos;
            this.length = length;
        }
    }

    // rootlevel of the node tree
    public partial class ParseTree : ParseNode
    {
        public ParseErrors Errors;

        public List<Token> Skipped;

        public ParseTree() : base(new Token(), "ParseTree")
        {
            Token.Type = TokenType.Start;
            Token.Text = "Root";
            Skipped = new List<Token>();
            Errors = new ParseErrors();
        }

        public string PrintTree()
        {
            StringBuilder sb = new StringBuilder();
            int indent = 0;
            PrintNode(sb, this, indent);
            return sb.ToString();
        }

        private void PrintNode(StringBuilder sb, ParseNode node, int indent)
        {
            
            string space = "".PadLeft(indent, ' ');

            sb.Append(space);
            sb.AppendLine(node.Text);

            foreach (ParseNode n in node.Nodes)
                PrintNode(sb, n, indent + 2);
        }
        
        /// <summary>
        /// this is the entry point for executing and evaluating the parse tree.
        /// </summary>
        /// <param name="paramlist">additional optional input parameters</param>
        /// <returns>the output of the evaluation function</returns>
        public object Eval(params object[] paramlist)
        {
            return Nodes[0].Eval(this, paramlist);
        }
    }

    public partial class ParseNode
    {
        protected string text;
        protected List<ParseNode> nodes;
        
        public List<ParseNode> Nodes { get {return nodes;} }
        
        public ParseNode Parent;
        public Token Token; // the token/rule
        public string Text { // text to display in parse tree 
            get { return text;} 
            set { text = value; }
        } 

        public virtual ParseNode CreateNode(Token token, string text)
        {
            ParseNode node = new ParseNode(token, text);
            node.Parent = this;
            return node;
        }

        protected ParseNode(Token token, string text)
        {
            this.Token = token;
            this.text = text;
            this.nodes = new List<ParseNode>();
        }

        protected object GetValue(ParseTree tree, TokenType type, int index)
        {
            return GetValue(tree, type, ref index);
        }

        protected object GetValue(ParseTree tree, TokenType type, ref int index)
        {
            object o = null;
            if (index < 0) return o;

            // left to right
            foreach (ParseNode node in nodes)
            {
                if (node.Token.Type == type)
                {
                    index--;
                    if (index < 0)
                    {
                        o = node.Eval(tree);
                        break;
                    }
                }
            }
            return o;
        }

        /// <summary>
        /// this implements the evaluation functionality, cannot be used directly
        /// </summary>
        /// <param name="tree">the parsetree itself</param>
        /// <param name="paramlist">optional input parameters</param>
        /// <returns>a partial result of the evaluation</returns>
        internal object Eval(ParseTree tree, params object[] paramlist)
        {
            object Value = null;

            switch (Token.Type)
            {
                case TokenType.Start:
                    Value = EvalStart(tree, paramlist);
                    break;
                case TokenType.Conditional:
                    Value = EvalConditional(tree, paramlist);
                    break;
                case TokenType.Identifier:
                    Value = EvalIdentifier(tree, paramlist);
                    break;
                case TokenType.BooleanExpr:
                    Value = EvalBooleanExpr(tree, paramlist);
                    break;
                case TokenType.CompareExpr:
                    Value = EvalCompareExpr(tree, paramlist);
                    break;
                case TokenType.AddExpr:
                    Value = EvalAddExpr(tree, paramlist);
                    break;
                case TokenType.MultExpr:
                    Value = EvalMultExpr(tree, paramlist);
                    break;
                case TokenType.PowerExpr:
                    Value = EvalPowerExpr(tree, paramlist);
                    break;
                case TokenType.Params:
                    Value = EvalParams(tree, paramlist);
                    break;
                case TokenType.Method:
                    Value = EvalMethod(tree, paramlist);
                    break;
                case TokenType.Array:
                    Value = EvalArray(tree, paramlist);
                    break;
                case TokenType.QualifiedName:
                    Value = EvalQualifiedName(tree, paramlist);
                    break;
                case TokenType.UnaryExpr:
                    Value = EvalUnaryExpr(tree, paramlist);
                    break;
                case TokenType.Atom:
                    Value = EvalAtom(tree, paramlist);
                    break;

                default:
                    Value = Token.Text;
                    break;
            }
            return Value;
        }

        protected virtual object EvalStart(ParseTree tree, params object[] paramlist)
        {
            return "Could not interpret input; no semantics implemented.";
        }

        protected virtual object EvalConditional(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalIdentifier(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalBooleanExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalCompareExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalAddExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalMultExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalPowerExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalParams(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalMethod(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalArray(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalQualifiedName(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalUnaryExpr(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }

        protected virtual object EvalAtom(ParseTree tree, params object[] paramlist)
        {
            throw new NotImplementedException();
        }


    }
    
    #endregion ParseTree
}
