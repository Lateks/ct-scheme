﻿using System;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Collections.Generic;

namespace CottontailSchemeLib
{
    public class CottontailSchemeException : Exception
    {
        public CottontailSchemeException(string msg) : base(msg) { }
    }

    public class TypeError : CottontailSchemeException
    {
        public TypeError(string functionName, string expectedType, string receivedType)
            : base(string.Format("{0}: contract violation:\n\texpected: {1}\n\tgiven:    {2}", functionName, expectedType, receivedType))
        { }
    }

    public class InvalidNumberOfArgsError : CottontailSchemeException
    {
        public InvalidNumberOfArgsError(string functionName, string expectedArgs, int receivedArgs)
            : base(string.Format("{0}: contract violation:\n\tinvalid number of arguments\n\texpected: {1}\n\tgiven:    {2}", functionName, expectedArgs, receivedArgs))
        { }
    }

    public class NotAProcedureError : CottontailSchemeException
    {
        public NotAProcedureError(CTObject obj)
            : base(string.Format("object of type {0} is not a procedure\ngiven: {1}", obj.DisplayType(), obj))
        { }
    }

    public class InternalError : CottontailSchemeException
    {
        public InternalError()
            : base("Internal error")
        { }
    }

    public class Constants
    {
        public static readonly CTObject Undefined = new CTUndefined();
        public static readonly CTObject True = new CTBool(true);
        public static readonly CTObject False = new CTBool(false);


        internal static CTObject ToCTBool(bool v)
        {
            if (v)
                return True;
            else
                return False;
        }
    }

    public class BuiltIns
    {
        private static readonly string NewlineFunctionName = "newline";
        private static readonly string DisplayFunctionName = "display";
        private static readonly string NotFunctionName = "not";
        private static readonly string AreEqFunctionName = "eq?";
        private static readonly string ListFunctionName = "list";
        private static readonly string ConsFunctionName = "cons";
        private static readonly string IsNullFunctionName = "null?";
        private static readonly string CdrFunctionName = "cdr";
        private static readonly string CarFunctionName = "car";
        private static readonly string PlusFunctionName = "+";
        private static readonly string MinusFunctionName = "-";
        private static readonly string DivFunctionName = "/";
        private static readonly string MultFunctionName = "*";
        private static readonly string IsZeroFunctionName = "zero?";
        private static readonly string LessThanFunctionName = "<";
        private static readonly string GreaterThanFunctionName = ">";
        private static readonly string MapFunctionName = "map";

        public static readonly CTObject ObjNewline = new CTDelegateProcedure0(Newline, NewlineFunctionName);
        public static readonly CTObject ObjDisplay = new CTDelegateProcedure1(Display, DisplayFunctionName);
        public static readonly CTObject ObjNot = new CTDelegateProcedure1(Not, NotFunctionName);
        public static readonly CTObject ObjAreEq = new CTDelegateProcedure2(AreEq, AreEqFunctionName);
        public static readonly CTObject ObjList = new CTDelegateProcedureVarargsArray(List, ListFunctionName);
        public static readonly CTObject ObjCons = new CTDelegateProcedure2(Cons, ConsFunctionName);
        public static readonly CTObject ObjIsNull = new CTDelegateProcedure1(IsNull, IsNullFunctionName);
        public static readonly CTObject ObjCdr = new CTDelegateProcedure1(Cdr, CdrFunctionName);
        public static readonly CTObject ObjCar = new CTDelegateProcedure1(Car, CarFunctionName);
        public static readonly CTObject ObjPlus = new CTDelegateProcedureVarargsArray(Plus, PlusFunctionName);
        public static readonly CTObject ObjMinus = new CTDelegateProcedureVarargsArray(Minus, MinusFunctionName);
        public static readonly CTObject ObjDiv = new CTDelegateProcedureVarargsArray(Div, DivFunctionName);
        public static readonly CTObject ObjMult = new CTDelegateProcedureVarargsArray(Mult, MultFunctionName);
        public static readonly CTObject ObjIsZero = new CTDelegateProcedure1(IsZero, IsZeroFunctionName);
        public static readonly CTObject ObjLessThan = new CTDelegateProcedureVarargsArray(LessThan, LessThanFunctionName);
        public static readonly CTObject ObjGreaterThan = new CTDelegateProcedureVarargsArray(GreaterThan, GreaterThanFunctionName);
        public static readonly CTObject ObjMap = new CTDelegateProcedureVarargsArray(Map, MapFunctionName);

        public static CTObject AreEq(CTObject a, CTObject b)
        {
            return Constants.ToCTBool(a.Equals(b));
        }

        public static CTObject Not(CTObject v)
        {
            bool b = v.ToBool();
            return Constants.ToCTBool(!b);
        }

        public static CTObject Display(CTObject v)
        {
            Console.Write(v);
            return Constants.Undefined;
        }

        public static CTObject Newline()
        {
            Console.Write("\n");
            return Constants.Undefined;
        }

        private static void AssertPair(string functionName, CTObject arg)
        {
            if (arg.GetType() != typeof(CTPair))
            {
                throw new TypeError(functionName, CTPair.TypeName, arg.DisplayType());
            }
        }

        public static CTObject List(IEnumerable<CTObject> vs)
        {
            IEnumerable<CTObject> reversed = vs.Reverse();
            CTObject lst = new CTEmptyList();
            foreach (CTObject obj in reversed)
            {
                lst = Cons(obj, lst);
            }
            return lst;
        }

        public static CTObject Car(CTObject v)
        {
            AssertPair(CarFunctionName, v);
            return ((CTPair)v).car;
        }

        public static CTObject Cdr(CTObject v)
        {
            AssertPair(CdrFunctionName, v);
            return ((CTPair)v).cdr;
        }

        public static CTObject Cons(CTObject a, CTObject b)
        {
            return new CTPair(a, b);
        }

        public static CTObject IsNull(CTObject v)
        {
            return Constants.ToCTBool(v.GetType() == typeof(CTEmptyList));
        }

        private static void AssertParameterCountAtLeast(string functionName, int n, CTObject[] args)
        {
            if (args.Length < n)
            {
                throw new InvalidNumberOfArgsError(functionName, "at least " + n, args.Length);
            }
        }

        private static void AssertNumber(string functionName, CTObject arg)
        {
            if (arg.GetType() != typeof(CTNumber))
            {
                throw new TypeError(functionName, CTNumber.TypeName, arg.DisplayType());
            }
        }

        private static double getValue(CTObject obj)
        {
            return ((CTNumber)obj).value;
        }

        private static void CheckAllArgsNumbers(string functionName, CTObject[] args)
        {
            foreach (var arg in args)
            {
                AssertNumber(functionName, arg);
            }
        }

        public static CTNumber Plus(CTObject[] args)
        {
            CheckAllArgsNumbers(PlusFunctionName, args);

            double sum = args.Select(getValue)
                             .Aggregate(0.0, (a, n) => a + n);

            return new CTNumber(sum);
        }

        public static CTNumber Minus(CTObject[] args)
        {
            CheckAllArgsNumbers(MinusFunctionName, args);
            AssertParameterCountAtLeast(MinusFunctionName, 1, args);

            double result = 0;
            if (args.Length == 1)
                result = -getValue(args[0]);
            else
                result = args.Skip(1)
                             .Select(getValue)
                             .Aggregate(getValue(args[0]), (a, n) => a - n);

            return new CTNumber(result);
        }

        public static CTNumber Div(CTObject[] args)
        {
            CheckAllArgsNumbers(DivFunctionName, args);
            AssertParameterCountAtLeast(DivFunctionName, 1, args);

            double result = 0;
            if (args.Length == 1)
            {
                
                result = 1.0 / getValue(args[0]);
            }
            else
            {
                result = args.Skip(1)
                                .Select(getValue)
                                .Aggregate(getValue(args[0]), (a, n) => a / n);
            }

            return new CTNumber(result);
        }

        public static CTNumber Mult(CTObject[] args)
        {
            CheckAllArgsNumbers(MultFunctionName, args);

            var result = args.Select(getValue)
                             .Aggregate(1.0, (a, n) => a * n);

            return new CTNumber(result);
        }

        public static CTObject IsZero(CTObject arg)
        {
            AssertNumber(IsZeroFunctionName, arg);
            var res = Math.Abs(((CTNumber)arg).value) <= double.Epsilon;
            return Constants.ToCTBool(res);
        }

        private static CTObject ComparePairs(CTObject[] args, Func<double, double, bool> compare)
        {
            var values = args.Select(getValue).ToArray();
            double prev = values[0];
            for (int i = 1; i < args.Length; ++i)
            {
                if (compare(prev, values[i]))
                {
                    prev = values[i];
                }
                else
                {
                    return Constants.False;
                }
            }
            return Constants.True;
        }

        public static CTObject LessThan(CTObject[] args)
        {
            CheckAllArgsNumbers(LessThanFunctionName, args);
            AssertParameterCountAtLeast(LessThanFunctionName, 1, args);
            return ComparePairs(args, (a, b) => a < b);
        }

        public static CTObject GreaterThan(CTObject[] args)
        {
            CheckAllArgsNumbers(GreaterThanFunctionName, args);
            AssertParameterCountAtLeast(GreaterThanFunctionName, 1, args);
            return ComparePairs(args, (a, b) => a > b);
        }

        public static CTObject Map(CTObject[] args)
        {
            AssertParameterCountAtLeast(MapFunctionName, 2, args);
            CTProcedure proc = args[0] as CTProcedure;

            if (proc == null)
            {
                throw new TypeError(MapFunctionName, CTProcedure.TypeName, args[0].DisplayType());
            }
            for (int i = 1; i < args.Length; ++i)
            {
                if (args[i].GetType() != typeof(CTPair))
                {
                    throw new TypeError(MapFunctionName, CTPair.TypeName, args[i].DisplayType());
                }
            }

            List<CTObject> results = new List<CTObject>();
            CTObject[] applyArgs = new CTObject[args.Length - 1];
            bool firstListEnded = false;
            while (!firstListEnded)
            {
                for (int i = 1; i < args.Length; ++i)
                {
                    if (args[i].GetType() == typeof(CTEmptyList))
                    {
                        firstListEnded = true;
                        break;
                    }
                    else if (args[i].GetType() == typeof(CTPair))
                    {
                        CTPair arg = (CTPair)args[i];
                        applyArgs[i - 1] = arg.car;
                        args[i] = arg.cdr;
                    }
                    else
                    {
                        throw new TypeError(MapFunctionName, CTPair.TypeName, args[i].DisplayType());
                    }
                }

                if (!firstListEnded)
                {
                    results.Add(Apply(proc, applyArgs));
                }
            }

            return List(results);
        }

        private static CTObject Apply(CTProcedure proc, CTObject[] args)
        {
            switch (args.Length)
            {
                case 0: return proc.apply0();
                case 1: return proc.apply1(args[0]);
                case 2: return proc.apply2(args[0], args[1]);
                case 3: return proc.apply3(args[0], args[1], args[2]);
                case 4: return proc.apply4(args[0], args[1], args[2], args[3]);
                case 5: return proc.apply5(args[0], args[1], args[2], args[3], args[4]);
                default: return proc.applyN(args);
            }
        }
    }

    public abstract class CTObject
    {
        public virtual bool ToBool() { return true; }
        public virtual CTObject ToCTBool() { return Constants.True; }
        public abstract string DisplayObject();

        public override string ToString()
        {
            return DisplayObject();
        }

        public virtual string REPLDisplayValue()
        {
            return DisplayObject();
        }

        public abstract string DisplayType();

        public override bool Equals(object obj)
        {
            return obj == this || (obj is CTObject) && IsEqualTo((CTObject)obj);
        }

        protected abstract bool IsEqualTo(CTObject obj);

        public virtual CTObject apply0()
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject apply1(CTObject a1)
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject apply2(CTObject a1, CTObject a2)
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject apply3(CTObject a1, CTObject a2, CTObject a3)
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject apply4(CTObject a1, CTObject a2, CTObject a3, CTObject a4)
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject apply5(CTObject a1, CTObject a2, CTObject a3, CTObject a4, CTObject a5)
        {
            throw new NotAProcedureError(this);
        }

        public virtual CTObject applyN(CTObject[] args)
        {
            throw new NotAProcedureError(this);
        }
    }

    public class CTUndefined : CTObject
    {
        internal static readonly string TypeName = "undefined";

        internal CTUndefined() { }

        public override string DisplayObject()
        {
            return "";
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTUndefined);
        }
    }

    public class CTBool : CTObject
    {
        internal static readonly string TypeName = "boolean";

        private bool value;

        internal CTBool(bool value)
        {
            this.value = value;
        }

        public override string DisplayObject()
        {
            if (value)
            {
                return "#t";
            } else
            {
                return "#f";
            }
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        public override bool ToBool()
        {
            return value;
        }

        public override CTObject ToCTBool()
        {
            return this;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTBool) && ((CTBool)obj).value == value;
        }
    }

    public class CTNumber : CTObject
    {
        internal static readonly string TypeName = "number";

        public readonly double value;

        public CTNumber(double value)
        {
            this.value = value;
        }

        public override string DisplayObject()
        {
            double fraction = value - (int)value;
            if (fraction > 0)
            {
                return value.ToString(CultureInfo.InvariantCulture);
            }
            else
            {
                return ((int)value).ToString();
            }
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTNumber) && ((CTNumber)obj).value == value;
        }
    }

    public class CTPair : CTObject
    {
        internal static readonly string TypeName = "pair";

        public readonly CTObject car;
        public readonly CTObject cdr;

        public CTPair(CTObject v1, CTObject v2)
        {
            car = v1;
            cdr = v2;
        }

        public override string DisplayObject()
        {
            StringBuilder repr = new StringBuilder("(");
            repr.Append(DisplayValue());
            CTObject tail = cdr;
            while (true)
            {
                if (tail.GetType() == typeof(CTPair))
                {
                    CTPair rest = (CTPair)tail;
                    repr.Append(" ").Append(rest.DisplayValue());
                    tail = rest.cdr;
                }
                else
                {
                    if (tail.GetType() != typeof(CTEmptyList))
                        repr.Append(" . ").Append(tail.DisplayObject());
                    break;
                }
            }

            repr.Append(")");

            return repr.ToString();
        }

        public string DisplayValue()
        {
            return car.DisplayObject();
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            if (obj.GetType() == typeof(CTPair))
            {
                CTPair other = (CTPair)obj;
                return other.car.Equals(car) && other.cdr.Equals(cdr);
            }
            else
            {
                return false;
            }
        }
    }

    public class CTEmptyList : CTObject
    {
        internal static readonly string TypeName = "()";

        public override string DisplayObject()
        {
            return "()";
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTEmptyList);
        }
    }

    public class CTString : CTObject
    {
        internal static readonly string TypeName = "string";

        public readonly string value;

        public CTString(string value)
        {
            this.value = value;
        }

        public override string DisplayObject()
        {
            return value;
        }

        public override string REPLDisplayValue()
        {
            return "\"" + value + "\"";
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTString) && ((CTString)obj).value.Equals(value);
        }
    }

    public class CTSymbol : CTObject
    {
        internal static readonly string TypeName = "symbol";

        public readonly string name;

        public CTSymbol(string name)
        {
            this.name = name;
        }

        public override string DisplayObject()
        {
            return name;
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj.GetType() == typeof(CTSymbol) && ((CTSymbol)obj).name.Equals(name);
        }
    }

    public class CTProcedure : CTObject
    {
        internal static readonly string TypeName = "procedure";
        private static int counter = 0;

        private readonly int identifier;
        private readonly string name;
        public int arity { get; private set; }
        public bool isVarargs { get; private set; }

        public CTProcedure(int arity)
        {
            this.arity = arity;
            ++counter;
            identifier = counter;
        }

        public CTProcedure(int arity, string name)
        {
            this.arity = arity;
            this.name = name;
        }

        // varargs functions
        public CTProcedure(string name)
        {
            this.name = name;
            isVarargs = true;
        }

        public CTProcedure()
        {
            isVarargs = true;
        }

        private string GetName()
        {
            return name != null ? name : DisplayObject();
        }

        public override string DisplayObject()
        {
            return string.Format("#<procedure:{0}>", name != null ? name : "anonymous" + identifier.ToString());
        }

        public override string DisplayType()
        {
            return TypeName;
        }

        protected override bool IsEqualTo(CTObject obj)
        {
            return obj == this;
        }

        public void matchNumArgs(int numArgs)
        {
            if (arity != numArgs)
            {
                throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), numArgs);
            }
        }

        public virtual CTObject funcall0()
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 0);
        }

        public virtual CTObject funcall1(CTObject a1)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 1);
        }

        public virtual CTObject funcall2(CTObject a1, CTObject a2)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 2);
        }

        public virtual CTObject funcall3(CTObject a1, CTObject a2, CTObject a3)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 3);
        }

        public virtual CTObject funcall4(CTObject a1, CTObject a2, CTObject a3, CTObject a4)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 4);
        }

        public virtual CTObject funcall5(CTObject a1, CTObject a2, CTObject a3, CTObject a4, CTObject a5)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), 5);
        }

        public virtual CTObject funcallN(CTObject[] args)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), args.Length);
        }

        public virtual CTObject funcallVarargs(CTObject[] args)
        {
            throw new InvalidNumberOfArgsError(GetName(), arity.ToString(), args.Length);
        }

        public override CTObject apply0()
        {
            if (isVarargs)
            {
                var args = new CTObject[0];
                return funcallVarargs(args);
            }
            else
            {
                return funcall0();
            }
        }

        public override CTObject apply1(CTObject a1)
        {
            if (isVarargs)
            {
                var args = new CTObject[] { a1 };
                return funcallVarargs(args);
            }
            else
            {
                return funcall1(a1);
            }
        }

        public override CTObject apply2(CTObject a1, CTObject a2)
        {
            if (isVarargs)
            {
                var args = new CTObject[] { a1, a2 };
                return funcallVarargs(args);
            }
            else
            {
                return funcall2(a1, a2);
            }
        }

        public override CTObject apply3(CTObject a1, CTObject a2, CTObject a3)
        {
            if (isVarargs)
            {
                var args = new CTObject[] { a1, a2, a3 };
                return funcallVarargs(args);
            }
            else
            {
                return funcall3(a1, a2, a3);
            }
        }

        public override CTObject apply4(CTObject a1, CTObject a2, CTObject a3, CTObject a4)
        {
            if (isVarargs)
            {
                var args = new CTObject[] { a1, a2, a3, a4 };
                return funcallVarargs(args);
            }
            else
            {
                return funcall4(a1, a2, a3, a4);
            }
        }

        public override CTObject apply5(CTObject a1, CTObject a2, CTObject a3, CTObject a4, CTObject a5)
        {
            if (isVarargs)
            {
                var args = new CTObject[] { a1, a2, a3, a4, a5 };
                return funcallVarargs(args);
            }
            else
            {
                return funcall5(a1, a2, a3, a4, a5);
            }
        }

        public override CTObject applyN(CTObject[] args)
        {
            if (isVarargs)
            {
                return funcallVarargs(args);
            }
            else
            {
                return funcallN(args);
            }
        }
    }

    public class CTDelegateProcedure0 : CTProcedure
    {
        private Func<CTObject> fun0;

        public CTDelegateProcedure0(Func<CTObject> f, string name)
            : base(0, name)
        {
            fun0 = f;
        }

        public CTDelegateProcedure0(Func<CTObject> f)
            : base(0)
        {
            fun0 = f;
        }

        public override CTObject funcall0()
        {
            return fun0();
        }
    }

    public class CTDelegateProcedure1 : CTProcedure
    {
        private Func<CTObject, CTObject> fun1;

        public CTDelegateProcedure1(Func<CTObject, CTObject> f, string name)
            : base(1, name)
        {
            fun1 = f;
        }

        public CTDelegateProcedure1(Func<CTObject, CTObject> f)
            : base(1)
        {
            fun1 = f;
        }

        public override CTObject funcall1(CTObject a1)
        {
            return fun1(a1);
        }
    }

    public class CTDelegateProcedure2 : CTProcedure
    {
        private Func<CTObject, CTObject, CTObject> fun2;

        public CTDelegateProcedure2(Func<CTObject, CTObject, CTObject> f, string name)
            : base(2, name)
        {
            fun2 = f;
        }

        public CTDelegateProcedure2(Func<CTObject, CTObject, CTObject> f)
            : base(2)
        {
            fun2 = f;
        }

        public override CTObject funcall2(CTObject a1, CTObject a2)
        {
            return fun2(a1, a2);
        }
    }

    public class CTDelegateProcedure3 : CTProcedure
    {
        private Func<CTObject, CTObject, CTObject, CTObject> fun3;

        public CTDelegateProcedure3(Func<CTObject, CTObject, CTObject, CTObject> f, string name)
            : base(3, name)
        {
            fun3 = f;
        }

        public CTDelegateProcedure3(Func<CTObject, CTObject, CTObject, CTObject> f)
            : base(3)
        {
            fun3 = f;
        }

        public override CTObject funcall3(CTObject a1, CTObject a2, CTObject a3)
        {
            return fun3(a1, a2, a3);
        }
    }

    public class CTDelegateProcedure4 : CTProcedure
    {
        private Func<CTObject, CTObject, CTObject, CTObject, CTObject> fun4;

        public CTDelegateProcedure4(Func<CTObject, CTObject, CTObject, CTObject, CTObject> f, string name)
            : base(4, name)
        {
            fun4 = f;
        }

        public CTDelegateProcedure4(Func<CTObject, CTObject, CTObject, CTObject, CTObject> f)
            : base(4)
        {
            fun4 = f;
        }

        public override CTObject funcall4(CTObject a1, CTObject a2, CTObject a3, CTObject a4)
        {
            return fun4(a1, a2, a3, a4);
        }
    }

    public class CTDelegateProcedure5 : CTProcedure
    {
        private Func<CTObject, CTObject, CTObject, CTObject, CTObject, CTObject> fun5;

        public CTDelegateProcedure5(Func<CTObject, CTObject, CTObject, CTObject, CTObject, CTObject> f, string name)
            : base(5, name)
        {
            fun5 = f;
        }

        public CTDelegateProcedure5(Func<CTObject, CTObject, CTObject, CTObject, CTObject, CTObject> f)
            : base(5)
        {
            fun5 = f;
        }

        public override CTObject funcall5(CTObject a1, CTObject a2, CTObject a3, CTObject a4, CTObject a5)
        {
            return fun5(a1, a2, a3, a4, a5);
        }
    }

    public class CTDelegateProcedureVarargsArray : CTProcedure
    {
        private Func<CTObject[], CTObject> funVarargs;

        public CTDelegateProcedureVarargsArray(Func<CTObject[], CTObject> f, string name)
            : base(name)
        {
            funVarargs = f;
        }

        public override CTObject funcallVarargs(CTObject[] args)
        {
            return funVarargs(args);
        }
    }

    public class CTDelegateProcedureVarargsList : CTProcedure
    {
        private Func<CTObject, CTObject> fun;

        public CTDelegateProcedureVarargsList(Func<CTObject, CTObject> f, string name)
            : base(name)
        {
            fun = f;
        }

        public CTDelegateProcedureVarargsList(Func<CTObject, CTObject> f)
        {
            fun = f;
        }

        public override CTObject funcallVarargs(CTObject[] args)
        {
            return fun(BuiltIns.List(args));
        }
    }

    public class ProcedureFrame
    {
        public virtual CTObject funcall0(int index) { throw new InternalError(); }
        public virtual CTObject funcall1(int index, CTObject a1) { throw new InternalError(); }
        public virtual CTObject funcall2(int index, CTObject a1, CTObject a2) { throw new InternalError(); }
        public virtual CTObject funcall3(int index, CTObject a1, CTObject a2, CTObject a3) { throw new InternalError(); }
        public virtual CTObject funcall4(int index, CTObject a1, CTObject a2, CTObject a3, CTObject a4) { throw new InternalError(); }
        public virtual CTObject funcall5(int index, CTObject a1, CTObject a2, CTObject a3, CTObject a4, CTObject a5) { throw new InternalError(); }
        public virtual CTObject funcallN(int index, CTObject[] args) { throw new InternalError(); }
        public virtual CTObject funcallVarargs(int index, CTObject args) { return Constants.Undefined; }
    }
}
