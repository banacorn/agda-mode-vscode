# Introduction

This is a Visual Studio Code extension for Agda. 

Having some frontend development skills will definitely help you contribute to this project (but don't worry, you'll manage even if you don't have them)

### Language

This project is written in [ReScript](https://rescript-lang.org/). It's essentially OCaml that compiles to JavaScript, so that we don't have to write JavaScript ourselves. 

### Framework

We use [React](https://rescript-lang.org/docs/react/latest/introduction) as the frontend framework. It comes with a nice binding for ReScript.

# Setup

You'll need to install [Node.js](https://nodejs.org/) for building the project. 

After cloning the files, download dependencies and build files with:

```bash
npm install 
npm run build
```

Fire up this command to enter the "watch mode" so that you don't have to rebuild stuff manually:

```bash 
npm run dev
```

Press <kbd>F5</kbd> in VS Code and you should have a extension development host with agda-mode running!

# Versioning Policy

This extension follows VS Code's recommended versioning scheme for extensions:

- **Release versions**: Use even minor numbers (e.g., `0.6.x`, `0.8.x`, `1.0.x`)
- **Prerelease versions**: Use odd minor numbers (e.g., `0.7.x`, `0.9.x`, `1.1.x`)

## Examples:
- `0.6.1` - Stable release
- `0.7.0` - First prerelease with new features (e.g., web support)
- `0.7.1` - Prerelease bug fixes and improvements
- `0.8.0` - Next stable release incorporating prerelease features

## Publishing:
- **Release versions** are published as stable extensions
- **Prerelease versions** are published with `preRelease: true` flag
- Users must explicitly opt-in to install prerelease versions

This versioning makes it clear to users which versions are stable and which are experimental.

