﻿// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="Martin Koníček" email="martin.konicek@gmail.com"/>
//     <version>$Revision$</version>
// </file>
using System;
using System.Collections.Generic;

namespace Debugger.AddIn.Visualizers.Common
{
	/// <summary>
	/// Provides sort ordering on ObjectProperties.
	/// </summary>
	class ObjectPropertyComparer : IComparer<ObjectProperty>
	{
		private static readonly ObjectPropertyComparer instance = new ObjectPropertyComparer();
		public static ObjectPropertyComparer Instance
		{
			get { return instance; }
		}
		
		public int Compare(ObjectProperty prop1, ObjectProperty prop2)
		{
			return prop1.Name.CompareTo(prop2.Name);
			
			// order by IsAtomic, Name - 
			// we now don't know whether a property is atomic until rendering it, 
			// so IsAtomic is always true when sorting in ObjectGraphBuilder
			/*int comparedAtomic = prop2.IsAtomic.CompareTo(prop1.IsAtomic);
			if (comparedAtomic != 0)
			{
				return comparedAtomic;
			}
			else
			{
				return prop1.Name.CompareTo(prop2.Name);
			}*/
		}
	}
}