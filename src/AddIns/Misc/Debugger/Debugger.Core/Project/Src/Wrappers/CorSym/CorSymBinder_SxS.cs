// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class CorSymBinder_SxS
	{
		
		private Debugger.Interop.CorSym.CorSymBinder_SxS wrappedObject;
		
		internal Debugger.Interop.CorSym.CorSymBinder_SxS WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public CorSymBinder_SxS(Debugger.Interop.CorSym.CorSymBinder_SxS wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(CorSymBinder_SxS));
		}
		
		public static CorSymBinder_SxS Wrap(Debugger.Interop.CorSym.CorSymBinder_SxS objectToWrap)
		{
			return new CorSymBinder_SxS(objectToWrap);
		}
		
		~CorSymBinder_SxS()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(CorSymBinder_SxS));
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
		
		public static bool operator ==(CorSymBinder_SxS o1, CorSymBinder_SxS o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(CorSymBinder_SxS o1, CorSymBinder_SxS o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			CorSymBinder_SxS casted = o as CorSymBinder_SxS;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
	}
}
