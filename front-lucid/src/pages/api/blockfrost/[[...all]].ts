import { NextApiHandler } from "next";
import httpProxyMiddleware from "next-http-proxy-middleware";

const blockfrostProxy: NextApiHandler = async (req, res) => {
	const target = process.env.BLOCKFROST_URL;
	const PROJECT_ID = process.env.BLOCKFROST_PROJECT_ID_PREPROD;
	if (PROJECT_ID === undefined || target == undefined) {
		throw new Error("Lucid Environment Variable Undefined");
	}
	try {
		await httpProxyMiddleware(req, res, {
			target,
			headers: {
				PROJECT_ID,
			},
			pathRewrite: [
				{
					patternStr: "^/api/blockfrost",
					replaceStr: "",
				},
			],
		});
	} catch (e) {
		console.error("Blockfrost proxy error", e);

		// NOTE(Alan): Not sure if this is compatible with Lucid / the Blockfrost provider
		return res.status(400).end();
	}
};

export default blockfrostProxy;
