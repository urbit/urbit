import React, { MouseEvent, useEffect, useMemo, useState } from 'react';
import { Link, useHistory } from 'react-router-dom';
import { Box, Icon, Image, Row } from '@tlon/indigo-react';
import UqbarLogo from '~/assets/img/uqbar-logo.png';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import useHarkState from '~/logic/state/hark';
import { useDmUnreads } from '~/logic/lib/useDmUnreads';
import { isMobileApp } from '~/logic/lib/platform';
import { bootstrapApi } from '~/logic/api/bootstrap';

const localSel = selectLocalState(['toggleOmnibox']);

type NavItemIcon = 'Home' | 'Messages' | 'Notifications' | 'ArrowRefresh' | 'Menu';

interface MobileNavItemProps {
  icon: NavItemIcon;
  selected?: boolean;
  notifications?: number;
}

interface NavItemLinkProps extends MobileNavItemProps {
  to: string;
}

function MobileNavItem({
  icon,
  notifications,
  selected = false
}: MobileNavItemProps) {
  return (
    <Box p={2}
      width="20vw"
      height="100%"
      display="flex"
      flexDirection="column"
      justifyContent="center"
      alignItems="center"
      position="relative"
      backgroundColor="washedGray"
    >
      {icon === 'Home' ? (
        <Image
          referrerPolicy="no-referrer"
          src={UqbarLogo}
          height="24px"
          width="24px"
          onError={console.warn}
        />
      ) : (
        <Icon size={24} icon={icon} borderBottom={selected ? '2px solid lightGray' : undefined} />
      )}
      {Boolean(notifications) && (
        <Box
          position="absolute" right="calc(50% - 18px)"
          top="4px" px="4px"
          py="1px"
          borderRadius="8px" backgroundColor="blue"
          fontSize="12px" color="white"
        >
          {notifications}
        </Box>
      )}
    </Box>
  );
}

export function MobileNavbar() {
  const history = useHistory();
  const { toggleOmnibox } = useLocalState(localSel);
  const { notificationsCount } = useHarkState();
  const { unreadDmCount } = useDmUnreads();
  const [currentPathname, setCurrentPathname] = useState(history.location.pathname);

  useEffect(() => {
    const { innerWidth, innerHeight } = window;
    document.write([
        '<style>',
        '.mobileNavbar { display: none; }',
        '@media screen and (orientation: portrait) and (min-height: ' + (Math.max(innerWidth, innerHeight) - 10) + 'px)',
        '{ .mobileNavbar { display: inherit; } }',
        '@media screen and (orientation: landscape) and (min-height: ' + (Math.min(innerWidth, innerHeight) - 30) + 'px)',
        '{ .mobileNavbar { display: inherit; } }',
        '</style>'
    ].join(' '));
  }, []);

  const options: NavItemLinkProps[] = useMemo(() => [
    {
      icon: 'Home',
      to: '/'
    },
    {
      icon: 'Messages',
      to: '/~landscape/messages'
    },
    {
      icon: 'Notifications',
      to: '/~notifications'
    },
    {
      icon: 'ArrowRefresh',
      to: '#'
    },
    {
      icon: 'Menu',
      to: '/'
    }
  ], []);

  const onClick = (icon: NavItemIcon, to: string) => (e: MouseEvent) => {
    if (icon === 'Menu' || icon === 'ArrowRefresh') {
      e.preventDefault();
      e.stopPropagation();

      if (icon === 'Menu') {
        toggleOmnibox();
      } else if (icon === 'ArrowRefresh') {
        bootstrapApi(true);
      }
    } else {
      setCurrentPathname(to);
    }
  };

  const getNotifications = (icon: string) : number => (icon === 'Notifications' && Math.max(0, notificationsCount - unreadDmCount))
    || (icon === 'Messages' && unreadDmCount)
    || 0;

  return (
    <Row className="mobileNavbar" backgroundColor="white" height={isMobileApp() ? '60px' : '50px'}>
      {options.map(({ icon, to }) => <Link key={icon} to={to} onClick={onClick(icon, to)}>
        <MobileNavItem icon={icon}
          selected={currentPathname.includes(to) && icon !== 'Menu'}
          notifications={getNotifications(icon)}
        />
      </Link>)}
    </Row>
  );
}
