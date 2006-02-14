// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISequentialStream
	{
		
		private Debugger.Interop.CorSym.ISequentialStream wrappedObject;
		
		internal Debugger.Interop.CorSym.ISequentialStream WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISequentialStream(Debugger.Interop.CorSym.ISequentialStream wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISequentialStream));
		}
		
		public static ISequentialStream Wrap(Debugger.Interop.CorSym.ISequentialStream objectToWrap)
		{
			return new ISequentialStream(objectToWrap);
		}
		
		~ISequentialStream()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISequentialStream));
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
		
		public static bool operator ==(ISequentialStream o1, ISequentialStream o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISequentialStream o1, ISequentialStream o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISequentialStream casted = o as ISequentialStream;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public uint RemoteRead(out byte pv, uint cb)
		{
			uint pcbRead;
			this.WrappedObject.RemoteRead(out pv, cb, out pcbRead);
			return pcbRead;
		}
		
		public uint RemoteWrite(ref byte pv, uint cb)
		{
			uint pcbWritten;
			this.WrappedObject.RemoteWrite(ref pv, cb, out pcbWritten);
			return pcbWritten;
		}
	}
}
