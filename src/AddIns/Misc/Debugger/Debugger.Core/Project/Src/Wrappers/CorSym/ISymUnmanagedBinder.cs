// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedBinder
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedBinder wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedBinder WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedBinder(Debugger.Interop.CorSym.ISymUnmanagedBinder wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedBinder));
		}
		
		public static ISymUnmanagedBinder Wrap(Debugger.Interop.CorSym.ISymUnmanagedBinder objectToWrap)
		{
			return new ISymUnmanagedBinder(objectToWrap);
		}
		
		~ISymUnmanagedBinder()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedBinder));
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
		
		public static bool operator ==(ISymUnmanagedBinder o1, ISymUnmanagedBinder o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedBinder o1, ISymUnmanagedBinder o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedBinder casted = o as ISymUnmanagedBinder;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public ISymUnmanagedReader GetReaderForFile(object importer, ref ushort filename, ref ushort searchPath)
		{
			return ISymUnmanagedReader.Wrap(this.WrappedObject.GetReaderForFile(importer, ref filename, ref searchPath));
		}
		
		public ISymUnmanagedReader GetReaderFromStream(object importer, IStream pstream)
		{
			return ISymUnmanagedReader.Wrap(this.WrappedObject.GetReaderFromStream(importer, pstream.WrappedObject));
		}
	}
}
