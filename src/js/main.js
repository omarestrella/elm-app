import { Elm } from '../app/Main.elm';
import { startAuth } from './auth';
import updatePlaidItem from './updatePlaidItem';

if (module && module.hot) {
  window.onerror = err => {
    if (/Error: \[elm-hot\]/.test(err)) {
      window.location.reload();
    }
  };
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: localStorage.getItem('accessToken') || null
});

app.ports.plaidLink.subscribe(function(message) {
  console.log('message', message);
  switch (message) {
    case 'StartLink':
      startAuth(app);
      break;
    default:
      console.warn('Received unhandled message:', message);
      break;
  }
});

app.ports.fixLoginError.subscribe(function(token) {
  if (!token) {
    console.warn('Tried to fix account without token:', { token });
    return;
  }

  updatePlaidItem(token);
});

app.ports.toStorage.subscribe(function(message) {
  console.log('Message', message);
  switch (message.type) {
    case 'setItem':
      localStorage.setItem(message.key, message.data);
      break;
    default:
      console.warn('Received unhandled storage message:', message);
  }
});
