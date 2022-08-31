import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { bootstrapApi } from './logic/api/bootstrap';
import './register-sw';
import './storage-wipe';
import App from './views/App';

// Start subscriptions as soon as possible before rendering anything
bootstrapApi();

ReactDOM.render(<App />, document.getElementById('root'));
