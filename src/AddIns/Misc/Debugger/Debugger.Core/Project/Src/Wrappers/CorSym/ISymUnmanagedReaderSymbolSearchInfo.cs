// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedReaderSymbolSearchInfo
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedReaderSymbolSearchInfo wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedReaderSymbolSearchInfo WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedReaderSymbolSearchInfo(Debugger.Interop.CorSym.ISymUnmanagedReaderSymbolSearchInfo wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedReaderSymbolSearchInfo));
		}
		
		public static ISymUnmanagedReaderSymbolSearchInfo Wrap(Debugger.Interop.CorSym.ISymUnmanagedReaderSymbolSearchInfo objectToWrap)
		{
			return new ISymUnmanagedReaderSymbolSearchInfo(objectToWrap);
		}
		
		~ISymUnmanagedReaderSymbolSearchInfo()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedReaderSymbolSearchInfo));
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
		
		public static bool operator ==(ISymUnmanagedReaderSymbolSearchInfo o1, ISymUnmanagedReaderSymbolSearchInfo o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedReaderSymbolSearchInfo o1, ISymUnmanagedReaderSymbolSearchInfo o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedReaderSymbolSearchInfo casted = o as ISymUnmanagedReaderSymbolSearchInfo;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public uint SymbolSearchInfoCount
		{
			get
			{
				uint pcSearchInfo;
				this.WrappedObject.GetSymbolSearchInfoCount(out pcSearchInfo);
				return pcSearchInfo;
			}
		}
		
		public ISymUnmanagedSymbolSearchInfo GetSymbolSearchInfo(uint cSearchInfo, out uint pcSearchInfo)
		{
			ISymUnmanagedSymbolSearchInfo rgpSearchInfo;
			Debugger.Interop.CorSym.ISymUnmanagedSymbolSearchInfo out_rgpSearchInfo;
			this.WrappedObject.GetSymbolSearchInfo(cSearchInfo, out pcSearchInfo, out out_rgpSearchInfo);
			rgpSearchInfo = ISymUnmanagedSymbolSearchInfo.Wrap(out_rgpSearchInfo);
			return rgpSearchInfo;
		}
	}
}
