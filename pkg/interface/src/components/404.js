import React from 'react';

const NotFound = () => <div
  className="fixed inter tc"
  style={{
    top: '50vh',
    left: '50%',
    transform: 'translate(-50%, -25vh)',
    fontFeatureSettings: '\'zero\' 1' }}>
  <h1 className="fw2 f2">404</h1>
  <p className="tc">Not found.<br /><br />
  If this is unexpected, email <code>support@tlon.io</code> or <a className="bb" href="https://github.com/urbit/urbit/issues/new/choose">submit an issue</a>.</p>
</div>;

export default NotFound;
