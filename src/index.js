import './main.css';
import { Elm } from './Main.elm';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    width: window.innerWidth,
    height: window.innerHeight,
    darkMode: window.matchMedia('(prefers-color-scheme: dark)').matches
  }
});