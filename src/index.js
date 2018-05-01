import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

let initData = {};
if (window.localStorage) {
  const data = JSON.parse (localStorage.getItem ('estimate-helper') || '{}');
  const id = Object.keys (data)[0];
  initData = data[id] || {items: [], hoursPerDay: 8, id: 0, nextId: 2};
}

const estimateHelperApp = Main.embed (document.getElementById ('root'), {
  initData,
});

estimateHelperApp.ports.saveEstimate.subscribe (estimate => {
  if (window.localStorage) {
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
