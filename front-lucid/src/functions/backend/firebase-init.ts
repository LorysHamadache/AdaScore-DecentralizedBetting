import { initializeApp } from "firebase/app";
import { getFirestore } from "firebase/firestore";
import { getFunctions } from "firebase/functions";

// TODO: Replace the following with your app's Firebase project configuration
const firebaseConfig = {
	apiKey: "AIzaSyA73QQOpwETcaMjI5urA7mAuTYnfWHvr-I",
	authDomain: "project-adascorepreprod.firebaseapp.com",
	projectId: "project-adascorepreprod",
	storageBucket: "project-adascorepreprod.appspot.com",
	messagingSenderId: "351875015171",
	appId: "1:351875015171:web:faf81183f2713eedce6436",
	measurementId: "G-82HH037JVE",
};

// Initialize Firebase
const app = initializeApp(firebaseConfig);
const firestore_db = getFirestore();
const firestore_functions = getFunctions(app, "europe-west1");

export { firestore_db, firestore_functions };
