import React from "react";

import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";

const UserBets = () => {
	const [data_betCreator, setDataBetCreator] = useState<DocumentData[]>([]);
	const [refreshCount, setRefreshCount] = useState(0);

	const handleRefresh = () => {
		setRefreshCount((prevCount) => prevCount + 1);
	};
	return null;
};
export default UserBets;

async function CancelBetTransaction() {
	return null;
}

function firebaseDatumToDatum(firebase_datum: DocumentData) {}
