// Import the functions you need from the SDKs you need
import { initializeApp } from "firebase/app";
import firebase from "firebase/app";
import { getFunctions, httpsCallable } from "firebase/functions";
import { getAnalytics } from "firebase/analytics";
import { FirebaseFunctions } from "@firebase/functions-types";
import {
	getFirestore,
	collection,
	getDocs,
	Firestore,
} from "firebase/firestore";

// TODO: Add SDKs for Firebase products that you want to use
// https://firebase.google.com/docs/web/setup#available-libraries

// Your web app's Firebase configuration
// For Firebase JS SDK v7.20.0 and later, measurementId is optional
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
