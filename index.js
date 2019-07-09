const ParcelProxyServer = require('parcel-proxy-server');
const express = require('express');

// configure the proxy server
const server = new ParcelProxyServer({
  entryPoint: './src/index.html',
  parcelOptions: {
    https: false
  },
  proxies: {
    '/api': {
      target: 'http://localhost:4000'
    }
  }
});

// the underlying parcel bundler is exposed on the server
// and can be used if needed
server.bundler.on('buildEnd', () => {
  console.log('📦 Build completed!');
});

// start up the server
server.listen(3000, () => { });
