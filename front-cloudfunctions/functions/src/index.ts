import * as functions from "firebase-functions";
import * as admin from "firebase-admin";
const cors = require("cors")({ origin: true });

admin.initializeApp();
const firestore = admin.firestore();

export const updateUser = functions
	.region("europe-west1")
	.https.onRequest(async (req, res) => {
		cors(req, res, async () => {
			const { address } = req.query;
			const now = new Date();
			const userRef = firestore.collection("user").doc(address as string);
			const userSnapshot = await userRef.get();

			if (userSnapshot.exists) {
				// Update existing document
				await userRef.update({
					number_connections: admin.firestore.FieldValue.increment(1),
					last_connection: now,
				});
			} else {
				// Create new document
				await userRef.set({
					number_connections: 1,
					last_connection: now,
				});
			}

			res.status(200).send("User updated successfully");
		});
	});

export const createBet = functions
	.region("europe-west1")
	.https.onCall(async (data, context) => {
		try {
			// Extract data from the request

			// Get the match document
			const matchRef = firestore.collection("match").doc(data.datum_matchid);
			const matchDoc = await matchRef.get();

			// Check if the match document exists
			if (!matchDoc.exists || matchRef.id != data.datum_matchid) {
				throw new functions.https.HttpsError("not-found", "Match not found");
			}

			const userRef = firestore.collection("user").doc(data.creator_ref);
			const userDoc = await userRef.get();

			// Check if the match document exists
			if (!userDoc.exists || userDoc.id != data.creator_ref) {
				throw new functions.https.HttpsError("not-found", "User not found");
			}
			// Create the bet document

			data.creator_ref = userRef;
			data.date = admin.firestore.FieldValue.serverTimestamp();
			data.updateddate = admin.firestore.FieldValue.serverTimestamp();
			data.match_ref = matchRef;
			data.status = "Creating";

			const betRef = firestore.collection("bet_offchain").doc(data.bet_txhash);
			await betRef.set(data);

			return { success: true };
		} catch (error) {
			console.error(error);
			throw new functions.https.HttpsError(
				"unknown",
				"An unknown error occurred"
			);
		}
	});

export const createRefScript = functions
	.region("europe-west1")
	.https.onCall(async (data, context) => {
		try {
			// Extract data from the request

			// Get the match document
			const matchRef = firestore.collection("match").doc(data.datum_matchid);
			const matchDoc = await matchRef.get();

			// Check if the match document exists
			if (!matchDoc.exists || matchRef.id != data.datum_matchid) {
				throw new functions.https.HttpsError("not-found", "Match not found");
			}

			const userRef = firestore.collection("user").doc(data.creator_ref);
			const userDoc = await userRef.get();

			// Check if the match document exists
			if (!userDoc.exists || userDoc.id != data.creator_ref) {
				throw new functions.https.HttpsError("not-found", "User not found");
			}
			// Create the bet document

			data.creator_ref = userRef;
			data.date = admin.firestore.FieldValue.serverTimestamp();
			data.updateddate = admin.firestore.FieldValue.serverTimestamp();
			data.match_ref = matchRef;
			data.status = "Creating";

			const betRef = firestore
				.collection("reference_script")
				.doc(data.bet_txhash);
			await betRef.set(data);

			return { success: true };
		} catch (error) {
			console.error(error);
			throw new functions.https.HttpsError(
				"unknown",
				"An unknown error occurred"
			);
		}
	});

export const cancelBet = functions
	.region("europe-west1")
	.https.onCall(async (data, context) => {
		try {
			// Extract data from the request
			const documentId = data.documentId;
			const txHash = data.txHash;

			// Get the bet document
			const betRef = firestore.collection("bet_offchain").doc(documentId);
			const betDoc = await betRef.get();

			// Check if the bet document exists
			if (!betDoc.exists || betRef.id != documentId) {
				throw new functions.https.HttpsError("not-found", "Bet not found");
			}

			// Update the bet document
			await betRef.update({
				cancel_txhash: txHash,
				status: "Cancelling",
				updateddate: admin.firestore.FieldValue.serverTimestamp(),
			});

			return { success: true };
		} catch (error) {
			console.error(error);
			throw new functions.https.HttpsError(
				"unknown",
				"An unknown error occurred"
			);
		}
	});
