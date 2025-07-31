// Empty module to replace Node.js core modules in web builds
// Provides mock implementations for commonly used Node.js functions
module.exports = {
  platform: () => 'web',
  arch: () => 'x64',
  EOL: '\n',
  // Add other commonly used OS functions as needed
  tmpdir: () => '/tmp',
  homedir: () => '/home',
  hostname: () => 'localhost',
  type: () => 'Web',
  release: () => '1.0.0',
  loadavg: () => [0, 0, 0],
  totalmem: () => 0,
  freemem: () => 0,
  cpus: () => [],
  networkInterfaces: () => ({}),
  uptime: () => 0,
  userInfo: () => ({ username: 'user', uid: -1, gid: -1, shell: null, homedir: '/home' })
};