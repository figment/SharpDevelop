// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedMethod
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedMethod wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedMethod WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedMethod(Debugger.Interop.CorSym.ISymUnmanagedMethod wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedMethod));
		}
		
		public static ISymUnmanagedMethod Wrap(Debugger.Interop.CorSym.ISymUnmanagedMethod objectToWrap)
		{
			return new ISymUnmanagedMethod(objectToWrap);
		}
		
		~ISymUnmanagedMethod()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedMethod));
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
		
		public static bool operator ==(ISymUnmanagedMethod o1, ISymUnmanagedMethod o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedMethod o1, ISymUnmanagedMethod o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedMethod casted = o as ISymUnmanagedMethod;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public uint Token
		{
			get
			{
				return this.WrappedObject.GetToken();
			}
		}
		
		public uint SequencePointCount
		{
			get
			{
				return this.WrappedObject.GetSequencePointCount();
			}
		}
		
		public ISymUnmanagedScope RootScope
		{
			get
			{
				return ISymUnmanagedScope.Wrap(this.WrappedObject.GetRootScope());
			}
		}
		
		public ISymUnmanagedScope GetScopeFromOffset(uint offset)
		{
			return ISymUnmanagedScope.Wrap(this.WrappedObject.GetScopeFromOffset(offset));
		}
		
		public uint GetOffset(ISymUnmanagedDocument document, uint line, uint column)
		{
			return this.WrappedObject.GetOffset(document.WrappedObject, line, column);
		}
		
		public void GetRanges(ISymUnmanagedDocument document, uint line, uint column, uint cRanges, out uint pcRanges, System.IntPtr ranges)
		{
			this.WrappedObject.GetRanges(document.WrappedObject, line, column, cRanges, out pcRanges, ranges);
		}
		
		public void GetParameters(uint cParams, out uint pcParams, System.IntPtr @params)
		{
			this.WrappedObject.GetParameters(cParams, out pcParams, @params);
		}
		
		public ISymUnmanagedNamespace Namespace
		{
			get
			{
				ISymUnmanagedNamespace pRetVal;
				Debugger.Interop.CorSym.ISymUnmanagedNamespace out_pRetVal;
				this.WrappedObject.GetNamespace(out out_pRetVal);
				pRetVal = ISymUnmanagedNamespace.Wrap(out_pRetVal);
				return pRetVal;
			}
		}
		
		public int GetSourceStartEnd(ISymUnmanagedDocument[] docs, uint[] lines, uint[] columns)
		{
			int pRetVal;
			Debugger.Interop.CorSym.ISymUnmanagedDocument[] array_docs = new Debugger.Interop.CorSym.ISymUnmanagedDocument[docs.Length];
			for (int i = 0; (i < docs.Length); i = (i + 1))
			{
				if ((docs[i] != null))
				{
					array_docs[i] = docs[i].WrappedObject;
				}
			}
			this.WrappedObject.GetSourceStartEnd(array_docs, lines, columns, out pRetVal);
			for (int i = 0; (i < docs.Length); i = (i + 1))
			{
				if ((array_docs[i] != null))
				{
					docs[i] = ISymUnmanagedDocument.Wrap(array_docs[i]);
				} else
				{
					docs[i] = null;
				}
			}
			return pRetVal;
		}
		
		public void GetSequencePoints(uint cPoints, out uint pcPoints, ref uint offsets, ref ISymUnmanagedDocument documents, ref uint lines, ref uint columns, ref uint endLines, ref uint endColumns)
		{
			Debugger.Interop.CorSym.ISymUnmanagedDocument ref_documents = documents.WrappedObject;
			this.WrappedObject.GetSequencePoints(cPoints, out pcPoints, ref offsets, ref ref_documents, ref lines, ref columns, ref endLines, ref endColumns);
			documents = ISymUnmanagedDocument.Wrap(ref_documents);
		}
	}
}
