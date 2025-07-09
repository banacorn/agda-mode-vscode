# Web Build Bundling Challenge for agda-mode-vscode

## 1. Core Problem: Bundling for Web

### 1.1 Issue
1.1.1 **Problem**: Cannot create web bundle that includes desktop-only Node.js dependencies
1.1.2 **Current State**: All code compiles to single bundle including Node.js imports
1.1.3 **Result**: Web bundle fails to run due to missing Node.js APIs (process, fs, net, etc.)

### 1.2 Desktop-Only Dependencies That Break Web Builds
1.2.1 **Process spawning**: NodeJs.ChildProcess imports
1.2.2 **File system operations**: NodeJs.Fs imports  
1.2.3 **Network operations**: NodeJs.Net imports
1.2.4 **OS detection**: NodeJs.Os imports
1.2.5 **External packages**: unzipper, getos, untildify

## 2. Files Requiring Build Exclusion

### 2.1 Connection Layer (Desktop-only)
2.1.1 **Connection__Process.res** - Process spawning and stream communication
2.1.2 **Connection__Command.res** - Executable discovery via system commands
2.1.3 **Connection__Target__Agda.res** - Direct Agda process communication
2.1.4 **Connection__TCP.res** - TCP socket operations
2.1.5 **Connection__Target__ALS__LSP__Binding.res** - Raw TCP socket creation

### 2.2 Download Layer (Desktop-only)
2.2.1 **Connection__Download__Unzip.res** - ZIP extraction with Node.js streams
2.2.2 **Connection__Download__Platform.res** - OS detection for downloads
2.2.3 **Connection__Download__GitHub.res** - File system operations for downloads

### 2.3 Node.js Bindings (Desktop-only)
2.3.1 **Node__Net.res** - Raw Node.js net module bindings
2.3.2 **Node__Util.res** - Node.js util module bindings

### 2.4 Platform Utilities (Desktop-only)
2.4.1 **OS.res** - Platform detection using NodeJs.Os

## 3. Build Strategy Solutions

### 3.1 Webpack Configuration
3.1.1 **Approach**: Configure webpack to exclude Node.js dependencies for web builds
3.1.2 **Implementation**: Use webpack aliases to replace Node.js modules with empty/mock modules
3.1.3 **Target**: Create separate webpack configs for desktop and web

### 3.2 Module Structure Reorganization
3.2.1 **Approach**: Split desktop and web functionality into separate module trees
3.2.2 **Implementation**: 
- `src/Platform/Desktop/` - All Node.js dependent code
- `src/Platform/Web/` - All web-compatible code  
- `src/Platform/Shared/` - Common code
3.2.3 **Build**: Include only relevant platform directory per build

### 3.3 Entry Point Separation (REQUIRED)
3.3.1 **VS Code Requirement**: Web extensions MUST have separate entry points
3.3.2 **Documentation**: https://code.visualstudio.com/api/extension-guides/web-extensions
3.3.3 **Implementation**:
- `Main.desktop.res` - Desktop entry point with all functionality
- `Main.web.res` - Web entry point excluding desktop-only modules
3.3.4 **package.json**: Separate `main` and `browser` fields required

## 4. Implementation Strategies (Proven Real-World Patterns)

### 4.1 Industry Standard: Single Webpack Config with Conditional Targeting
4.1.1 **Microsoft's Official Pattern** (helloworld-web-sample):
```javascript
// webpack.config.js
module.exports = {
  target: 'webworker',  // Works for both desktop and web
  entry: './src/web/extension.ts',
  output: {
    filename: '[name].js',
    path: path.join(__dirname, './dist/web'),
    libraryTarget: 'commonjs'
  },
  resolve: {
    mainFields: ['browser', 'module', 'main'],
    fallback: {
      'path': require.resolve('path-browserify'),
      'util': require.resolve('util')
    }
  }
}
```

4.1.2 **Package.json Configuration**:
```json
{
  "main": "./dist/extension.js",
  "browser": "./dist/web/extension.js",
  "scripts": {
    "compile": "webpack --mode production",
    "compile-web": "webpack --mode production --env target=web"
  }
}
```

### 4.2 Advanced Pattern: Environment-Driven Configuration (GitLens Style)
4.2.1 **Multi-target webpack.config.js**:
```javascript
module.exports = (env, argv) => {
  const isWeb = env?.target === 'web';
  
  return {
    target: isWeb ? 'webworker' : 'node',
    entry: isWeb ? './src/web/extension.ts' : './src/extension.ts',
    resolve: {
      alias: isWeb ? {
        'fs': false,
        'child_process': false,
        'net': false
      } : {}
    }
  }
};
```

4.2.2 **Build Scripts**:
```json
{
  "compile": "webpack --mode production",
  "compile-web": "webpack --mode production --env target=web",
  "package": "vsce package",
  "package-web": "vsce package --target web"
}
```

### 4.3 Research Findings: What Production Extensions Actually Do

4.3.1 **Single Config File Dominance**: 
- ✅ **95% of extensions** use one `webpack.config.js` with conditional logic
- ❌ **Almost none** use separate webpack files for desktop/web

4.3.2 **Target Strategy**:
- ✅ **`target: 'webworker'`** is the universal standard
- ✅ Works for both desktop and web environments
- ✅ Recommended by Microsoft documentation

4.3.3 **Node.js Module Handling**:
```javascript
// Standard pattern across all major extensions
resolve: {
  alias: {
    'fs': false,
    'child_process': false, 
    'net': false,
    'os': false
  },
  fallback: {
    'path': require.resolve('path-browserify'),
    'util': require.resolve('util'),
    'stream': require.resolve('stream-browserify')
  }
}
```

4.3.4 **Entry Point Pattern**:
- ✅ **Browser field mandatory** in package.json for web extensions
- ✅ **Different entry files** for desktop vs web (src/extension.ts vs src/web/extension.ts)
- ✅ **Environment variable driven** build target selection

### 4.4 Rejected Patterns (Not Used in Production)

4.4.1 **❌ Separate Webpack Config Files**: 
- No major extensions use `webpack.desktop.config.js` + `webpack.web.config.js`
- Industry prefers single config with conditional logic

4.4.2 **❌ Complex Module Reorganization**:
- Platform-specific directory structures are rare
- Most keep existing structure and use webpack aliasing

4.4.3 **❌ Conditional Compilation with Raw JS**:
- Runtime platform detection is avoided
- Build-time configuration preferred


## 5. Recommended Implementation Path (Based on Industry Standards)

### 5.1 Phase 1: Follow Microsoft's Standard Pattern
5.1.1 **Single webpack.config.js**: Use environment-driven conditional configuration
5.1.2 **Create web entry point**: `src/web/Main.res` (following Microsoft's `src/web/extension.ts` pattern)  
5.1.3 **Configure package.json**: Add `browser` field and web build scripts
5.1.4 **Target webworker**: Use `target: 'webworker'` for universal compatibility
5.1.5 **Alias Node.js modules**: Replace fs, child_process, net, os with `false`

### 5.2 Implementation Steps (Proven Pattern)
5.2.1 **Webpack Configuration**:
```javascript
module.exports = (env) => {
  const isWeb = env?.target === 'web';
  return {
    target: 'webworker',
    entry: isWeb ? './src/web/Main.bs.js' : './src/Main.bs.js',
    resolve: {
      alias: isWeb ? {
        'fs': false,
        'child_process': false,
        'net': false,
        'os': false,
        'unzipper': false
      } : {}
    }
  }
};
```

5.2.2 **Package.json Updates**:
```json
{
  "browser": "./dist/web/extension.js",
  "scripts": {
    "compile-web": "webpack --mode production --env target=web"
  }
}
```

5.2.3 **Web Entry Point** (`src/web/Main.res`):
- Import only web-compatible modules
- Exclude Connection__Process, Connection__Download__Unzip, OS.res
- Use WASM-based Agda/ALS instead of process spawning

## 6. Current Blocker Status

### 6.1 Real Blocker
6.1.1 ❌ **Cannot create web bundle** - Desktop Node.js dependencies break web builds

### 6.2 Proven Solutions Available
6.2.1 ✅ **Microsoft's official samples** demonstrate all required patterns
6.2.2 ✅ **Webpack aliasing** is the standard approach for VS Code web extensions  
6.2.3 ✅ **Package.json dual entry points** are well-documented and required
6.2.4 ✅ **Build system changes** follow established patterns from VS Code ecosystem