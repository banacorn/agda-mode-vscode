#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');

console.log("🌐 Starting VS Code Web Extension Development with Watch Mode");
console.log("=============================================================");

// Initial build
console.log("1. Initial web extension build...");
const buildProcess = spawn('sh', ['-c', 'npx rescript && npx lessc style/style.less dist/style.css && npx webpack --mode production --env target=web'], {
    cwd: __dirname,
    stdio: 'inherit'
});

buildProcess.on('close', (code) => {
    if (code === 0) {
        console.log("✅ Initial build successful");
        console.log("2. Starting watch processes...");
        
        // Start the watch build process
        const watchProcess = spawn('npx', [
            'rescript', 'build', '-w'
        ], {
            cwd: __dirname,
            stdio: 'pipe'
        });
        
        // Start less watch compiler
        const lessProcess = spawn('npx', [
            'less-watch-compiler', 'style/', 'dist/'
        ], {
            cwd: __dirname,
            stdio: 'pipe'
        });
        
        // Start webpack watch
        const webpackProcess = spawn('npx', [
            'webpack', '--mode', 'development', '--watch', '--env', 'target=web'
        ], {
            cwd: __dirname,
            stdio: 'pipe'
        });
        
        console.log("🔄 Watch processes started");
        console.log("3. Starting VS Code Web...");
        
        // Start VS Code web using the official test-web tool
        const webProcess = spawn('vscode-test-web', [
            '--extensionDevelopmentPath=.',
            '--quality=stable',
            '--port=3000',
            '--browser=none'
        ], {
            stdio: 'inherit'
        });
        
        console.log("🚀 VS Code Web starting...");
        console.log("   - ReScript, Less, and Webpack are watching for changes");
        console.log("   - Extension reloads automatically on changes");
        console.log("");
        console.log("🌐 Open VS Code Web in your browser:");
        console.log("   http://localhost:3000");
        console.log("");
        console.log("📋 Once VS Code Web loads:");
        console.log("   1. Open Command Palette (Ctrl+Shift+P)");
        console.log("   2. Search for 'Agda: Hello Web (Test Command)'");
        console.log("   3. Run the command to test your extension");
        console.log("");
        console.log("🔧 Full development workflow with watch mode!");
        console.log("Press Ctrl+C to stop all processes");
        
        // Handle graceful shutdown
        process.on('SIGINT', () => {
            console.log("\n👋 Stopping all processes...");
            watchProcess.kill();
            lessProcess.kill();
            webpackProcess.kill();
            webProcess.kill();
            process.exit(0);
        });
        
        webProcess.on('error', (err) => {
            console.error('❌ VS Code Web failed to start:', err);
        });
        
    } else {
        console.log("❌ Initial build failed");
        process.exit(1);
    }
});

buildProcess.on('error', (err) => {
    console.error('❌ Build error:', err);
    process.exit(1);
});