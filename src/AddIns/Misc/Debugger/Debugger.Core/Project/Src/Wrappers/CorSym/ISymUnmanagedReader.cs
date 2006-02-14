// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

namespace Debugger.Wrappers.CorSym
{
	using System;
	
	
	public class ISymUnmanagedReader
	{
		
		private Debugger.Interop.CorSym.ISymUnmanagedReader wrappedObject;
		
		internal Debugger.Interop.CorSym.ISymUnmanagedReader WrappedObject
		{
			get
			{
				return this.wrappedObject;
			}
		}
		
		public ISymUnmanagedReader(Debugger.Interop.CorSym.ISymUnmanagedReader wrappedObject)
		{
			this.wrappedObject = wrappedObject;
			ResourceManager.TrackCOMObject(wrappedObject, typeof(ISymUnmanagedReader));
		}
		
		public static ISymUnmanagedReader Wrap(Debugger.Interop.CorSym.ISymUnmanagedReader objectToWrap)
		{
			return new ISymUnmanagedReader(objectToWrap);
		}
		
		~ISymUnmanagedReader()
		{
			object o = wrappedObject;
			wrappedObject = null;
			ResourceManager.ReleaseCOMObject(o, typeof(ISymUnmanagedReader));
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
		
		public static bool operator ==(ISymUnmanagedReader o1, ISymUnmanagedReader o2)
		{
			return ((object)o1 == null && (object)o2 == null) ||
			       ((object)o1 != null && (object)o2 != null && o1.WrappedObject == o2.WrappedObject);
		}
		
		public static bool operator !=(ISymUnmanagedReader o1, ISymUnmanagedReader o2)
		{
			return !(o1 == o2);
		}
		
		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
		
		public override bool Equals(object o)
		{
			ISymUnmanagedReader casted = o as ISymUnmanagedReader;
			return (casted != null) && (casted.WrappedObject == wrappedObject);
		}
		
		
		public ISymUnmanagedDocument GetDocument(ref ushort url, System.Guid language, System.Guid languageVendor, System.Guid documentType)
		{
			return ISymUnmanagedDocument.Wrap(this.WrappedObject.GetDocument(ref url, language, languageVendor, documentType));
		}
		
		public void GetDocuments(uint cDocs, out uint pcDocs, System.IntPtr pDocs)
		{
			this.WrappedObject.GetDocuments(cDocs, out pcDocs, pDocs);
		}
		
		public uint UserEntryPoint
		{
			get
			{
				return this.WrappedObject.GetUserEntryPoint();
			}
		}
		
		public ISymUnmanagedMethod GetMethod(uint token)
		{
			return ISymUnmanagedMethod.Wrap(this.WrappedObject.GetMethod(token));
		}
		
		public ISymUnmanagedMethod GetMethodByVersion(uint token, int version)
		{
			return ISymUnmanagedMethod.Wrap(this.WrappedObject.GetMethodByVersion(token, version));
		}
		
		public void GetVariables(uint parent, uint cVars, out uint pcVars, System.IntPtr pVars)
		{
			this.WrappedObject.GetVariables(parent, cVars, out pcVars, pVars);
		}
		
		public void GetGlobalVariables(uint cVars, out uint pcVars, System.IntPtr pVars)
		{
			this.WrappedObject.GetGlobalVariables(cVars, out pcVars, pVars);
		}
		
		public ISymUnmanagedMethod GetMethodFromDocumentPosition(ISymUnmanagedDocument document, uint line, uint column)
		{
			return ISymUnmanagedMethod.Wrap(this.WrappedObject.GetMethodFromDocumentPosition(document.WrappedObject, line, column));
		}
		
		public void GetSymAttribute(uint parent, ref ushort name, uint cBuffer, out uint pcBuffer, System.IntPtr buffer)
		{
			this.WrappedObject.GetSymAttribute(parent, ref name, cBuffer, out pcBuffer, buffer);
		}
		
		public void GetNamespaces(uint cNameSpaces, out uint pcNameSpaces, System.IntPtr namespaces)
		{
			this.WrappedObject.GetNamespaces(cNameSpaces, out pcNameSpaces, namespaces);
		}
		
		public void Initialize(object importer, ref ushort filename, ref ushort searchPath, IStream pIStream)
		{
			this.WrappedObject.Initialize(importer, ref filename, ref searchPath, pIStream.WrappedObject);
		}
		
		public void UpdateSymbolStore(ref ushort filename, IStream pIStream)
		{
			this.WrappedObject.UpdateSymbolStore(ref filename, pIStream.WrappedObject);
		}
		
		public void ReplaceSymbolStore(ref ushort filename, IStream pIStream)
		{
			this.WrappedObject.ReplaceSymbolStore(ref filename, pIStream.WrappedObject);
		}
		
		public void GetSymbolStoreFileName(uint cchName, out uint pcchName, System.IntPtr szName)
		{
			this.WrappedObject.GetSymbolStoreFileName(cchName, out pcchName, szName);
		}
		
		public void GetMethodsFromDocumentPosition(ISymUnmanagedDocument document, uint line, uint column, uint cMethod, out uint pcMethod, System.IntPtr pRetVal)
		{
			this.WrappedObject.GetMethodsFromDocumentPosition(document.WrappedObject, line, column, cMethod, out pcMethod, pRetVal);
		}
		
		public int GetDocumentVersion(ISymUnmanagedDocument pDoc, out int version)
		{
			int pbCurrent;
			this.WrappedObject.GetDocumentVersion(pDoc.WrappedObject, out version, out pbCurrent);
			return pbCurrent;
		}
		
		public int GetMethodVersion(ISymUnmanagedMethod pMethod)
		{
			int version;
			this.WrappedObject.GetMethodVersion(pMethod.WrappedObject, out version);
			return version;
		}
	}
}
