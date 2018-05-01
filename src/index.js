import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const estimateHelperApp = Main.embed (document.getElementById ('root'));

estimateHelperApp.ports.saveEstimate.subscribe (estimate => {
  if (window.localStorage) {
    console.log (estimate);
    try {
      const data = JSON.parse (localStorage.getItem ('estimate-helper')) || {};
      data[estimate.id] = estimate;
      localStorage.setItem ('estimate-helper', JSON.stringify (data));
      estimateHelperApp.ports.receiveNotification.send ({
        type: 'Success',
        message: 'Saved successfully.',
      });
    } catch (e) {
      estimateHelperApp.ports.receiveNotification.send ({
        type: 'Error',
        message: 'Unexpected Error :: ' + e.message,
      });
    }
  } else {
    alert ('localStorage is not supported');
    estimateHelperApp.ports.receiveNotification.send ({
      type: 'Error',
      message: 'Error : localStorage is not supported.',
    });
  }
});

registerServiceWorker ();
