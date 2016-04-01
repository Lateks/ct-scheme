using System;

namespace CottontailSchemeLib
{
    public class Constants
    {
        public static readonly CTObject Undefined = new CTUndefined();
        public static readonly CTObject True = new CTBool(true);
        public static readonly CTObject False = new CTBool(false);
    }

    public abstract class CTObject
    {
        public abstract bool ToBool();
        public abstract CTObject ToCTBool();
        public abstract String Display();

        public override string ToString()
        {
            return Display();
        }
    }

    public class CTUndefined : CTObject
    {
        internal CTUndefined() { }

        public override string Display()
        {
            return "";
        }

        public override bool ToBool()
        {
            return true;
        }

        public override CTObject ToCTBool()
        {
            throw new NotImplementedException();
        }
    }

    public class CTBool : CTObject
    {
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

        public override bool ToBool()
        {
            return value;
        }

        public override CTObject ToCTBool()
        {
            return this;
        }
    }
}
