using System;

namespace CottontailSchemeLib
{
    public class TypeError : Exception
    {
        public TypeError(string functionName, string expectedType, string receivedType)
            : base(String.Format("{0}: contract violation:\n\texpected: {1}\n\tgiven: {2}", functionName, expectedType, receivedType))
        { }
    }

    public class Constants
    {
        public static readonly CTObject Undefined = new CTUndefined();
        public static readonly CTObject True = new CTBool(true);
        public static readonly CTObject False = new CTBool(false);
    }

    public class ListOperations
    {
        static string CdrFunctionName = "cdr";
        static string CarFunctionName = "car";

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
    }

    public class CTNumber : CTObject
    {
        internal static readonly string TypeName = "number";

        private double value;

        public CTNumber(double value)
        {
            this.value = value;
        }

        public override string Display()
        {
            double fraction = value - (int)value;
            if (fraction > 0) // Double.Epsilon?
            {
                return value.ToString();
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

        // TODO: use a string builder and test
        public override string Display()
        {
            string repr = "(" + DisplayValue();
            CTObject tail = cdr;
            while (true)
            {
                if (tail.GetType() == typeof(CTPair))
                {
                    CTPair rest = (CTPair)tail;
                    repr += " " + rest.DisplayValue();
                    tail = rest.Cdr();
                }
                else if (tail.GetType() != typeof(CTEmptyList))
                {
                    repr += " . " + tail.Display();
                }
                else
                {
                    break;
                }
            }

            repr += ")";

            return repr;
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
    }

    // TODO: procedure objects
    // - remember typename
}
