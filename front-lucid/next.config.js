/** @type {import('next').NextConfig} */
const nextConfig = {
	reactStrictMode: true,
	webpack: function (config, options) {
		config.experiments = {
			topLevelAwait: true,
			asyncWebAssembly: true,
			layers: true, // optional, required with some bundlers/frameworks
		};
		return config;
	},
};
module.exports = nextConfig;
