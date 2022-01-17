import { Box, Col, Text } from '@tlon/indigo-react';
import React, { ReactNode, useEffect } from 'react';
import Helmet from 'react-helmet';
import { useLocation } from 'react-router-dom';
import useHarkState from '~/logic/state/hark';
import { PropFunc } from '~/types';
import { SidebarItem as BaseSidebarItem } from '~/views/landscape/components/SidebarItem';
import { CalmPrefs } from './components/lib/CalmPref';
import DebugPane from './components/lib/Debug';
import DisplayForm from './components/lib/DisplayForm';
import { LeapSettings } from './components/lib/LeapSettings';
import { NotificationPreferences } from './components/lib/NotificationPref';
import S3Form from './components/lib/S3Form';
import SecuritySettings from './components/lib/Security';
import { DmSettings } from './components/lib/DmSettings';
import ShortcutSettings from './components/lib/ShortcutSettings';

export const Skeleton = (props: { children: ReactNode }) => (
  <Box height='100%' width='100%' px={[0, 3]} pb={[0, 3]} borderRadius={1}>
    <Box
      display='grid'
      gridTemplateColumns={[
        '100%',
        'minmax(150px, 1fr) 3fr',
        'minmax(250px, 1fr) 4fr'
      ]}
      gridTemplateRows='100%'
      height='100%'
      width='100%'
      borderRadius={2}
      bg='white'
      border={1}
      borderColor='lightGray'
    >
      {props.children}
    </Box>
  </Box>
);

type ProvSideProps = 'to' | 'selected';
type BaseProps = PropFunc<typeof BaseSidebarItem>;
function SidebarItem(props: { hash: string } & Omit<BaseProps, ProvSideProps>) {
  const { hash, icon, text, ...rest } = props;

  const to = `/~settings#${hash}`;

  const location = useLocation();
  const selected = location.hash.slice(1) === hash;

  return (
    <BaseSidebarItem
      {...rest}
      icon={icon}
      text={text}
      to={to}
      selected={selected}
    />
  );
}

function SettingsItem(props: { children: ReactNode }) {
  const { children } = props;

  return (
    <Box borderBottom={1} borderBottomColor='lightGray'>
      {children}
    </Box>
  );
}

export default function SettingsScreen(props: any) {
  const location = useLocation();
  const hash = location.hash.slice(1);
  const notificationsCount = useHarkState(state => state.notificationsCount);

  useEffect(() => {
    const debugShower = (event) => {
      if (hash)
return;
      if (event.key === '~') {
        window.location.hash = 'debug';
      }
    };
    document.addEventListener('keyup', debugShower);

    return () => {
      document.removeEventListener('keyup', debugShower);
    };
  }, [hash]);

  return (
    <>
      <Helmet defer={false}>
        <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Settings</title>
      </Helmet>
      <Skeleton>
        <Col
          height='100%'
          borderRight={1}
          borderRightColor='lightGray'
          display={hash === '' ? 'flex' : ['none', 'flex']}
          width='100%'
          overflowY='auto'
        >
          <Text display='block' mt={4} mb={3} mx={3} fontSize={2} fontWeight='700'>
            System Preferences
          </Text>
          <Col>
            <SidebarItem
              icon='Notifications'
              text='Notifications'
              hash='notifications'
            />
            <SidebarItem icon='Image' text='Display' hash='display' />
            <SidebarItem icon='Upload' text='Remote Storage' hash='s3' />
            <SidebarItem icon='LeapArrow' text='Leap' hash='leap' />
            <SidebarItem icon='Messages' text='Direct Messages' hash='dm' />
            <SidebarItem icon='Node' text='CalmEngine' hash='calm' />
            <SidebarItem icon='EastCarat' text='Shortcuts' hash='shortcuts' />
            <SidebarItem
              icon='Locked'
              text='Devices + Security'
              hash='security'
            />
          </Col>
        </Col>
        <Col flexGrow={1} overflowY='auto'>
          <SettingsItem>
            {hash === 'notifications' && (
              <NotificationPreferences
                {...props}
                graphConfig={props.notificationsGraphConfig}
              />
            )}
            {hash === 'display' && <DisplayForm />}
            {hash === 'dm' && <DmSettings />}
            {hash === 'shortcuts' && <ShortcutSettings />}
            {hash === 's3' && <S3Form  />}
            {hash === 'leap' && <LeapSettings />}
            {hash === 'calm' && <CalmPrefs />}
            {hash === 'security' && <SecuritySettings />}
            {hash === 'debug' && <DebugPane />}
          </SettingsItem>
        </Col>
      </Skeleton>
    </>
  );
}
