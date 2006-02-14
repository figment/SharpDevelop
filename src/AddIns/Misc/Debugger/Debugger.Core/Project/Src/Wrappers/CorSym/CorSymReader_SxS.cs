// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class CorSymReader_SxS
	{
		
		private Debugger.Interop.CorSym.CorSymReader_SxS wrappedObject;
		
		internal Debugger.Interop.CorSym.CorSymReader_SxS WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public CorSymReader_SxS(Debugger.Interop.CorSym.CorSymReader_SxS wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(CorSymReader_SxS));
		}
		
		public static CorSymReader_SxS Wrap(Debugger.Interop.CorSym.CorSymReader_SxS objectToWrap)
		{
			return new CorSymReader_SxS(objectToWrap);
		}
		
		~CorSymReader_SxS()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(CorSymReader_SxS));
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
		
		public static bool operator ==(CorSymReader_SxS o1, CorSymReader_SxS o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(CorSymReader_SxS o1, CorSymReader_SxS o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			CorSymReader_SxS casted = o as CorSymReader_SxS;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
	}
}
