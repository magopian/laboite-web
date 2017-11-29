import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const root = document.getElementById('root');
Main.embed(root);

root.addEventListener('click', () => {
  root.requestFullscreen();
});

registerServiceWorker();
