// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedDocumentWriter
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedDocumentWriter wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedDocumentWriter WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedDocumentWriter(Debugger.Interop.CorSym.ISymUnmanagedDocumentWriter wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedDocumentWriter));
		}
		
		public static ISymUnmanagedDocumentWriter Wrap(Debugger.Interop.CorSym.ISymUnmanagedDocumentWriter objectToWrap)
		{
			return new ISymUnmanagedDocumentWriter(objectToWrap);
		}
		
		~ISymUnmanagedDocumentWriter()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedDocumentWriter));
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
		
		public static bool operator ==(ISymUnmanagedDocumentWriter o1, ISymUnmanagedDocumentWriter o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedDocumentWriter o1, ISymUnmanagedDocumentWriter o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedDocumentWriter casted = o as ISymUnmanagedDocumentWriter;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public void SetSource(uint sourceSize, ref byte source)
		{
			this.WrappedObject.SetSource(sourceSize, ref source);
		}
		
		public void SetCheckSum(System.Guid algorithmId, uint checkSumSize, ref byte checkSum)
		{
			this.WrappedObject.SetCheckSum(algorithmId, checkSumSize, ref checkSum);
		}
	}
}
