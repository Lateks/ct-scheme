using System;
using System.Globalization;
using System.Linq;
using System.Text;

namespace CottontailSchemeLib
{
    public class TypeError : Exception
    {
        public TypeError(string functionName, string expectedType, string receivedType)
            : base(string.Format("{0}: contract violation:\n\texpected: {1}\n\tgiven: {2}", functionName, expectedType, receivedType))
        { }
    }

    public class Constants
    {
        public static readonly CTObject Undefined = new CTUndefined();
        public static readonly CTObject True = new CTBool(true);
        public static readonly CTObject False = new CTBool(false);
    }

    public class GenericOperations
    {
        public static string GetDisplayValue(CTObject obj)
        {
            if (obj.GetType() == typeof(CTString))
            {
                return ((CTString)obj).value;
            }
            else
            {
                return obj.ToString();
            }
        }
    }

    public class ListOperations
    {
        private static readonly string CdrFunctionName = "cdr";
        private static readonly string CarFunctionName = "car";

        private static void AssertPair(string functionName, CTObject arg)
        {
            if (arg.GetType() != typeof(CTPair))
            {
                throw new TypeError(functionName, CTPair.TypeName, arg.DisplayType());
            }
        }

        public static CTObject List(CTObject[] vs)
        {
            CTObject lst = new CTEmptyList();
            for (int i = vs.Length - 1; i >= 0; --i)
            {
                lst = Cons(vs[i], lst);
            }
            return lst;
        }

        public static CTObject Car(CTObject v)
        {
            AssertPair(CarFunctionName, v);
            return ((CTPair)v).Car();
        }

        public static CTObject Cdr(CTObject v)
        {
            AssertPair(CdrFunctionName, v);
            return ((CTPair)v).Cdr();
        }

        public static CTObject Cons(CTObject a, CTObject b)
        {
            return new CTPair(a, b);
        }

        public static bool IsNull(CTObject v)
        {
            return v.GetType() == typeof(CTEmptyList);
        }
    }

    public class NumberOperations
    {
        private static readonly string PlusFunctionName = "+";
        private static readonly string MinusFunctionName = "-";
        private static readonly string DivFunctionName = "/";
        private static readonly string MultFunctionName = "*";
        private static readonly string IsZeroFunctionName = "zero?";
        private static readonly string LessThanFunctionName = "<";
        private static readonly string GreaterThanFunctionName = ">";

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

        private static void CheckArgs(string functionName, CTObject[] args)
        {
            foreach (var arg in args)
            {
                AssertNumber(functionName, arg);
            }
        }

        public static CTNumber Plus(CTObject[] args)
        {
            CheckArgs(PlusFunctionName, args);

            double sum = args.Select(getValue)
                             .Aggregate(0.0, (a, n) => a + n);

            return new CTNumber(sum);
        }

        public static CTNumber Minus(CTObject[] args)
        {
            CheckArgs(MinusFunctionName, args);

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
            CheckArgs(DivFunctionName, args);

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
            CheckArgs(MultFunctionName, args);

            var result = args.Select(getValue)
                             .Aggregate(1.0, (a, n) => a * n);

            return new CTNumber(result);
        }

        public static bool IsZero(CTObject arg)
        {
            AssertNumber(IsZeroFunctionName, arg);
            return Math.Abs(((CTNumber)arg).value) <= double.Epsilon;
        }

        private static bool ComparePairs(CTObject[] args, Func<double, double, bool> compare)
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
                    return false;
                }
            }
            return true;
        }

        public static bool LessThan(CTObject[] args)
        {
            CheckArgs(LessThanFunctionName, args);
            return ComparePairs(args, (a, b) => a < b);
        }

        public static bool GreaterThan(CTObject[] args)
        {
            CheckArgs(GreaterThanFunctionName, args);
            return ComparePairs(args, (a, b) => a > b);
        }
    }

    public abstract class CTObject
    {
        public virtual bool ToBool() { return true; }
        public virtual CTObject ToCTBool() { return Constants.True; }
        public abstract string Display();

        public override string ToString()
        {
            return Display();
        }

        public abstract string DisplayType();

        public override bool Equals(object obj)
        {
            return obj == this || (obj is CTObject) && IsEqualTo((CTObject)obj);
        }

        protected abstract bool IsEqualTo(CTObject obj);
    }

    public class CTUndefined : CTObject
    {
        internal static readonly string TypeName = "undefined";

        internal CTUndefined() { }

        public override string Display()
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

        public override string Display()
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

        public double value { get; private set; }

        public CTNumber(double value)
        {
            this.value = value;
        }

        public override string Display()
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

        private CTObject car;
        private CTObject cdr;

        public CTPair(CTObject v1, CTObject v2)
        {
            car = v1;
            cdr = v2;
        }

        public override string Display()
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
                    tail = rest.Cdr();
                }
                else
                {
                    if (tail.GetType() != typeof(CTEmptyList))
                        repr.Append(" . ").Append(tail.Display());
                    break;
                }
            }

            repr.Append(")");

            return repr.ToString();
        }

        public string DisplayValue()
        {
            return car.Display();
        }

        public CTObject Car()
        {
            return car;
        }

        public CTObject Cdr()
        {
            return cdr;
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

        public override string Display()
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

        public string value { get; private set; }

        public CTString(string value)
        {
            this.value = value;
        }

        public override string Display()
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

        private readonly string name;

        public CTSymbol(string name)
        {
            this.name = name;
        }

        public override string Display()
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

    // TODO: procedure objects
    // - remember typename
    public class CTProcedure : CTObject
    {
        internal static readonly string TypeName = "procedure";
        private static int counter = 0;

        private readonly int identifier;
        private readonly string name;

        public CTProcedure()
        {
            ++counter;
            identifier = counter;
        }

        public CTProcedure(string name)
        {
            this.name = name;
        }

        public override string Display()
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
    }
}
