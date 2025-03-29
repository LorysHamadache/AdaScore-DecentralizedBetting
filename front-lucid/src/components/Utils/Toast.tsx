// Toast.tsx
import React, {
	createContext,
	useReducer,
	ReactNode,
	useContext,
	useEffect,
	useState,
} from "react";

// Define the type of the toast object
interface ToastType {
	id: string;
	message: string;
	color: string;
}

// Define the type of the state
type State = ToastType[];

// Define the type of the action
type Action =
	| { type: "ADD_TOAST"; payload: ToastType }
	| { type: "REMOVE_TOAST"; payload: string };

// Define the type of the context
interface ToastContextType {
	toasts: State;
	addToast: (toast: ToastType) => void;
	removeToast: (id: string) => void;
}

// Create the context
export const ToastContext = createContext<ToastContextType | undefined>(
	undefined
);

// Define the reducer
const toastReducer = (state: State, action: Action): State => {
	switch (action.type) {
		case "ADD_TOAST":
			return [...state, action.payload];
		case "REMOVE_TOAST":
			return state.filter((toast) => toast.id !== action.payload);
		default:
			return state;
	}
};

// Create the context provider
export const ToastProvider: React.FC<{ children: ReactNode }> = ({
	children,
}) => {
	const [toasts, dispatch] = useReducer(toastReducer, []);

	const addToast = (toast: ToastType) => {
		dispatch({ type: "ADD_TOAST", payload: toast });
	};

	const removeToast = (id: string) => {
		dispatch({ type: "REMOVE_TOAST", payload: id });
	};

	return (
		<ToastContext.Provider value={{ toasts, addToast, removeToast }}>
			{children}
		</ToastContext.Provider>
	);
};

interface ToastProps {
	toast: { id: string; message: string; color: string };
}

// Define the Toast component
export const Toast: React.FC<ToastProps> = ({ toast }) => {
	const { removeToast } = useContext(ToastContext)!;
	const classn = `toast ${toast.color}`;
	const [timerId, setTimerId] = useState<NodeJS.Timeout | null>(null);

	useEffect(() => {
		const timer = setTimeout(() => {
			removeToast(toast.id);
		}, 4000); // 15000 milliseconds = 15 seconds
		setTimerId(timerId);

		// Clear the timer when the component is unmounted
		return () => {
			if (timerId) {
				clearTimeout(timerId);
			}
		};
	}, [removeToast, toast.id]);

	return (
		<div className={classn}>
			<div>{toast.message}</div>
			<button
				type="button"
				className="ml-auto -mx-1.5 -my-1.5 rounded-lg  p-1.5 inline-flex h-8 w-8"
				data-dismiss-target="#toast-default"
				onClick={() => removeToast(toast.id)}
			>
				<svg className="w-5 h-5" viewBox="0 0 20 20">
					<path d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"></path>
				</svg>
			</button>
		</div>
	);
};

// Define the ToastContainer component
export const ToastContainer: React.FC = () => {
	const { toasts } = useContext(ToastContext)!;

	return (
		<div className="toast-container">
			{toasts.map((toast) => (
				<Toast key={toast.id} toast={toast} />
			))}
		</div>
	);
};
