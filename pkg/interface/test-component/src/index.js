import React, { Component } from 'react';
import classnames from 'classnames';

import { TestComponent } from '/components/test-component';
import { PeerComponent } from '/components/peer-component';

window.componentTable = {
  testComponent: TestComponent,
  peerComponent: PeerComponent,
  ...window.componentTable,
}
