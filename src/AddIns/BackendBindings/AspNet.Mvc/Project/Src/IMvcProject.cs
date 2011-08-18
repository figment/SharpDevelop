﻿// Copyright (c) AlphaSierraPapa for the SharpDevelop Team (for details please see \doc\copyright.txt)
// This code is distributed under the GNU LGPL (for details please see \doc\license.txt)

using System;
using System.Collections.Generic;
using ICSharpCode.SharpDevelop;
using ICSharpCode.SharpDevelop.Project;

namespace ICSharpCode.AspNet.Mvc
{
	public interface IMvcProject
	{
		IProject Project { get; }
		string RootNamespace { get; }
		
		void Save();
		MvcTextTemplateLanguage GetTemplateLanguage();
		IEnumerable<IMvcClass> GetModelClasses();
	}
}
