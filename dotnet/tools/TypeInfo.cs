using System;

namespace HsWrapGen
{
	/// <summary>
	/// Given a type name, locate the metainfo needed to generate
	/// Haskell wrappers.
	/// </summary>
    public class TypeInfo {
        protected Type m_type;
        protected System.Reflection.MemberInfo[] m_members;

        public System.Type Type {
            get { return (m_type); }
        }

        public System.Reflection.MemberInfo[] Members {
            get { return (m_members); }
        }   

		private bool myFilter(System.Reflection.MemberInfo m,
							  System.Object filterCrit)
		{
			return
			   (m.MemberType == System.Reflection.MemberTypes.Method ||
				m.MemberType == System.Reflection.MemberTypes.Property ||
				m.MemberType == System.Reflection.MemberTypes.Field);
		}

		public TypeInfo(System.String tyName)
		{
			m_type = System.Type.GetType(tyName);

			m_members = m_type.FindMembers(System.Reflection.MemberTypes.All,
										   System.Reflection.BindingFlags.DeclaredOnly |
										   System.Reflection.BindingFlags.Instance |
										   System.Reflection.BindingFlags.Public |
										   System.Reflection.BindingFlags.Static,
										   new System.Reflection.MemberFilter(myFilter),
										   null);
		}
	}
}
