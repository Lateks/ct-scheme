using System;
using System.Globalization;
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

        private double value;

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

        private readonly string value;

        public CTString(string value)
        {
            this.value = value;
        }

        public string GetValue()
        {
            return value;
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
}
