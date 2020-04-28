import React from 'react';
import { useLocation, Link } from 'react-router-dom';

import GroupFilter from './GroupFilter';
// import { Sigil } from "/components/lib/icons/sigil";

const getLocationName = (basePath) => {
  if (basePath === '~chat')
return 'Chat';
  // if (path === '~chat') return 'Publish';
};

const Sigil = () => (<div />);

const StatusBar = (props) => {
  const location = useLocation();
  const basePath = location.pathname.split('/')[1];
  const locationName = location.pathname === '/'
    ? 'Home'
    : getLocationName(basePath);

  const popout = window.location.href.includes('popout/')
    ? 'dn' : 'dn db-m db-l db-xl';

  const invites = (props.invites && props.invites['/contacts'])
    ? props.invites['/contacts']
    : {};

  return (
    <div
      className={
        'bg-white bg-gray0-d w-100 justify-between relative tc pt3 ' + popout
      }
      style={{ height: 45 }}
    >
      <div className="fl lh-copy absolute left-0" style={{ top: 8 }}>
      <a href="/~groups/me" className="dib v-top">
          <Sigil
            ship={'~' + props.ship}
            classes="v-mid mix-blend-diff"
            size={16}
            color={'#000000'}
          />
      </a>
      <GroupFilter invites={invites} associations={props.associations} />
      <span className="dib f9 v-mid gray2 ml1 mr1 c-default inter">/</span>
        {
          location.pathname === '/'
            ? null
            : <Link
                className="dib f9 v-mid inter ml2 no-underline"
                to="/"
                style={{ top: 14 }}
              >
              ⟵
              </Link>
        }
         <p className="dib f9 v-mid inter ml2 white-d">{locationName}</p>
      </div>
    </div>
  );
};

export default StatusBar;
