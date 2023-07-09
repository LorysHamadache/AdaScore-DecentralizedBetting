import {
	collection,
	getDocs,
	getDoc,
	Firestore,
	doc,
	where,
	query,
} from "firebase/firestore";

async function getMatchs(db: Firestore) {
	const matchsCollection = collection(db, "match");
	const matchsSnapshot = await getDocs(matchsCollection);
	return matchsSnapshot;
}

async function getMatchDetails(db: Firestore, match_id: string) {
	const matchRef = doc(db, "match", match_id);
	const matchSnapshot = await getDoc(matchRef);
	console.log(match_id);
	return matchSnapshot;
}

async function getMyBetsCreator1(db: Firestore, user_id: string) {
	const betRef = collection(db, "bet_offchain");
	const userRef = doc(db, "user", user_id);
	const betQuery = query(betRef, where("creator_ref", "==", userRef));
	const betSnapshot = await getDocs(betQuery);
	console.log(user_id);
	return betSnapshot;
}

async function getMyBetsCreator(db: Firestore, user_id: string) {
	const betRef = collection(db, "bet_offchain");
	const userRef = doc(db, "user", user_id);
	const betQuery = query(betRef, where("creator_ref", "==", userRef));
	const betSnapshot = await getDocs(betQuery);

	const betsData = [];

	for (const betDoc of betSnapshot.docs) {
		const betData = betDoc.data();
		const matchRef = betData.match_ref; // Replace 'matchRef' with the actual field name for the match reference

		if (matchRef) {
			try {
				const matchDoc = await getDoc(matchRef);
				betData.match_data = matchDoc.data();
				betData.id = betDoc.id;
			} catch (error) {
				console.error(
					`Error fetching match data for bet ${betDoc.id}: `,
					error
				);
			}
		}

		betsData.push(betData);
	}

	console.log(betsData);
	return betsData;
}

export { getMatchs, getMatchDetails, getMyBetsCreator };
