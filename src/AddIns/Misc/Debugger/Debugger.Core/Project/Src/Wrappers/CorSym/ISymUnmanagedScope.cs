// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedScope
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedScope wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedScope WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedScope(Debugger.Interop.CorSym.ISymUnmanagedScope wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedScope));
		}
		
		public static ISymUnmanagedScope Wrap(Debugger.Interop.CorSym.ISymUnmanagedScope objectToWrap)
		{
			return new ISymUnmanagedScope(objectToWrap);
		}
		
		~ISymUnmanagedScope()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedScope));
		}
		
		public bool Is<T>() where T: class
		{
			try {
				CastTo<T>();
				return true;
			} catch {
				return false;
			}
		}
		
		public T As<T>() where T: class
		{
			try {
				return CastTo<T>();
			} catch {
				return null;
			}
		}
		
		public T CastTo<T>() where T: class
		{
			return (T)Activator.CreateInstance(typeof(T), this.WrappedObject);
		}
		
		public static bool operator ==(ISymUnmanagedScope o1, ISymUnmanagedScope o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedScope o1, ISymUnmanagedScope o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedScope casted = o as ISymUnmanagedScope;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public ISymUnmanagedMethod Method
		{
			get
			{
				return ISymUnmanagedMethod.Wrap(this.WrappedObject.GetMethod());
			}
		}
		
		public ISymUnmanagedScope Parent
		{
			get
			{
				return ISymUnmanagedScope.Wrap(this.WrappedObject.GetParent());
			}
		}
		
		public void GetChildren(uint cChildren, out uint pcChildren, System.IntPtr children)
		{
			this.WrappedObject.GetChildren(cChildren, out pcChildren, children);
		}
		
		public uint StartOffset
		{
			get
			{
				return this.WrappedObject.GetStartOffset();
			}
		}
		
		public uint EndOffset
		{
			get
			{
				return this.WrappedObject.GetEndOffset();
			}
		}
		
		public uint LocalCount
		{
			get
			{
				return this.WrappedObject.GetLocalCount();
			}
		}
		
		public void GetLocals(uint cLocals, out uint pcLocals, System.IntPtr locals)
		{
			this.WrappedObject.GetLocals(cLocals, out pcLocals, locals);
		}
		
		public void GetNamespaces(uint cNameSpaces, out uint pcNameSpaces, System.IntPtr namespaces)
		{
			this.WrappedObject.GetNamespaces(cNameSpaces, out pcNameSpaces, namespaces);
		}
	}
}
