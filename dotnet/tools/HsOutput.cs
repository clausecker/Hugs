//
// (c) sof, 2002-2003
//
using System;

namespace HsWrapGen
{
	/// <summary>
	/// 
	/// </summary>
	public class HsOutput
	{
        private System.Type m_type;
		private System.Reflection.MemberInfo[] m_members;
        private System.Collections.Specialized.StringCollection m_names;
        private System.Collections.Specialized.StringCollection m_imports;

	public HsOutput(System.Type ty,System.Reflection.MemberInfo[] mems) {
	  m_type = ty;
	  m_members = mems;
	  m_names   = new System.Collections.Specialized.StringCollection();
	  m_imports = new System.Collections.Specialized.StringCollection();
	}

        protected void OutputHeader(System.IO.StreamWriter st) {
            st.WriteLine("module {0} where", m_type.FullName);
            st.WriteLine("");
            st.WriteLine("import DotNet");
            st.WriteLine("import qualified {0}", m_type.BaseType.FullName);
            foreach (String s in m_imports) {
              st.WriteLine("import qualified {0}", s);
            }
            st.WriteLine("");
	    // ToDo: provide the option of stashing this away in a separate
	    //       module.
            st.WriteLine("data {0}_ a", m_type.Name);
            st.WriteLine("type {0} a = {1}.{2} ({0}_ a)",
                         m_type.Name,
                         m_type.BaseType.FullName,
                         m_type.BaseType.Name);
            st.WriteLine("");
        }

        private String ToHaskellName(String x) {
            System.String candName, candNameOrig;
            System.Int32 uniq = 1;
            if (System.Char.IsUpper(x[0])) {
                candName = 
                    String.Concat(System.Char.ToLower(x[0]),
                    x.Substring(1));
            } else {
                candName = x;
            }
            candNameOrig = candName;
            while (m_names.Contains(candName)) {
                candName = String.Concat(candNameOrig,"_",uniq.ToString());
                uniq++;
            }
            m_names.Add(candName);

            return candName;
        }

        private String ToHaskellConName(String x) {
            System.String candName, candNameOrig;
            System.Int32 uniq = 1;
            if (System.Char.IsLower(x[0])) {
                candName = 
                    String.Concat(System.Char.ToUpper(x[0]),
                    x.Substring(1));
            } else {
                candName = x;
            }
            candNameOrig = candName;
            while (m_names.Contains(candName)) {
                candName = String.Concat(candNameOrig,"_",uniq.ToString());
                uniq++;
            }
            m_names.Add(candName);

            return candName;
        }

        private void AddImport(System.String nm) {

            if (!m_imports.Contains(nm) && String.Compare(nm, m_type.FullName) != 0) {
                m_imports.Add(nm);
            }
        }

        protected void OutputHaskellType(System.Text.StringBuilder sb,
                                         System.Type ty,
                                         System.Int32 idx) {
            if (ty.FullName == "System.Int32") {
              sb.Append("Int"); return;
            }
            if (ty.FullName == "System.Boolean") {
              sb.Append("Bool"); return;
            }
            if (ty.FullName == "System.String") {
                sb.Append("String"); return;
            }
            if (ty.FullName == "System.Char") {
              sb.Append("Char"); return;
            }
            if (ty.FullName == "System.Void") {
              sb.Append("()"); return;
            }
            if (ty.FullName == "System.Object") {
                sb.AppendFormat("System.Object.Object a{0}",idx); return;
            }

            if (ty.IsArray) {
                String eltNm = ty.GetElementType().FullName;
                AddImport("System.Array");
                AddImport(eltNm);
                sb.AppendFormat("System.Array.Array ({0}.{1} a{2})", eltNm, ty.GetElementType().Name, idx);
            } else {
                AddImport(ty.FullName);
                sb.AppendFormat("{0}.{1} a{2}", ty.FullName, ty.Name, idx);
            }       
        }

        protected void OutputMethodSig(System.Text.StringBuilder sb,
                                       System.Reflection.MemberInfo mi) {
            System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
            System.Reflection.ParameterInfo[] ps = m.GetParameters();
            int i;

            for (i=0; i < ps.Length; i++) {
                OutputHaskellType(sb,ps[i].ParameterType,i);
                sb.Append(" -> ");
            }
	    if (m.IsStatic) {
	      sb.Append("IO (");
	    } else {
	      sb.AppendFormat("{0} obj -> IO (", mi.DeclaringType.Name);
	    }
            OutputHaskellType(sb,m.ReturnType,i);
            sb.AppendFormat("){0}",System.Environment.NewLine);
        }

        protected void OutputArgs(System.Text.StringBuilder sb,
                                  System.Reflection.MemberInfo mi,
                                  System.Boolean isTupled) {
            System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
            Int32 i = 0;
            System.Reflection.ParameterInfo[] ps = m.GetParameters();

            if (isTupled && ps.Length != 1) sb.Append("(");

            for (i=0; i < ps.Length; i++) {
                sb.AppendFormat("arg{0}",i); 
                if (isTupled && (i+1) < ps.Length) {
                    sb.Append(",");
                } else {
                    if (!isTupled) sb.Append(" ");
                }
            }
            if (isTupled && ps.Length != 1) sb.Append(")");
        }

        protected void OutputMember(System.Text.StringBuilder sb,
				    System.Reflection.MemberInfo mi) {
            switch (mi.MemberType) {
                case System.Reflection.MemberTypes.Method:
                    System.String methName = ToHaskellName(mi.Name);
		    System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
		    sb.Append("foreign import dotnet");
                    sb.AppendFormat("{0}",System.Environment.NewLine);
		    // the 'method' bit is really optional.
		    sb.AppendFormat("  \"{0}method {1}.{2}\"", (m.IsStatic ? "static " : ""), mi.DeclaringType, mi.Name);
                    sb.AppendFormat("{0}",System.Environment.NewLine);
		    sb.AppendFormat("  {0} :: ", methName);
		    OutputMethodSig(sb,mi);
                    // the mind boggles, System.Environment ?
                    sb.AppendFormat("{0}",System.Environment.NewLine);
                    break;
                default:
                    break;
            }
        }
        
        protected void OutputField(System.Text.StringBuilder sb,
				   System.Reflection.MemberInfo mi) {
            switch (mi.MemberType) {
                case System.Reflection.MemberTypes.Field:
                    System.String fieldName = ToHaskellConName(mi.Name);
		    sb.Append(fieldName);
                    break;
                default:
                    break;
            }
        }

        public void OutputToFile(String fn) {
            System.IO.FileStream fs = new System.IO.FileStream(fn,System.IO.FileMode.Create);
            System.IO.StreamWriter st = new System.IO.StreamWriter(fs,System.Text.Encoding.ASCII);
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

	    if (String.Compare(m_type.BaseType.FullName, "System.Enum") == 0) {
	      /* enumerations are mapped onto Haskell data types. */
	      System.String sep = " = ";
	      sb.AppendFormat("data {0}Ty", m_type.Name);
	      sb.Append(System.Environment.NewLine);
	      foreach (System.Reflection.MemberInfo mem in m_members) {
		sb.Append(sep);
                OutputField(sb,mem);
		sb.Append(System.Environment.NewLine);
		sep = " | ";
	      }
	      sb.AppendFormat("  deriving ( Enum, Show, Read ){0}",System.Environment.NewLine);
	      // Emit functions for converting betw alg type and object type.
	      AddImport("IOExts");
	      AddImport("System.Type");
	      AddImport("System.Enum");
	      sb.AppendFormat("to{0} :: {0}Ty -> {0} (){1}", m_type.Name, System.Environment.NewLine);
	      sb.AppendFormat("to{0} tag = IOExts.unsafePerformIO (System.Enum.parse (IOExts.unsafePerformIO (System.Type.getType \"{1}\")) (show tag)){2}", m_type.Name, m_type.AssemblyQualifiedName,System.Environment.NewLine);
	      sb.Append(System.Environment.NewLine);
	      sb.AppendFormat("from{0} :: {0} () -> {0}Ty{1}", m_type.Name, System.Environment.NewLine);
	      sb.AppendFormat("from{0} obj = IOExts.unsafePerformIO (toString obj >>= return.read)", m_type.Name);
	      sb.Append(System.Environment.NewLine);
	    } else {
	      foreach (System.Reflection.MemberInfo mem in m_members) {
                OutputMember(sb,mem);
	      }
	    }
	    
 
            OutputHeader(st);
            st.WriteLine(sb.ToString());
            st.Flush();
            st.Close();
            fs.Close();
	    }
	}
}
