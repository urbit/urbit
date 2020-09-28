import { string } from 'prop-types';
import React, { Component } from 'react';
import { manifestURL } from '~/logic/lib/util';

interface PWAIcon {
  src: string;
  sizes: string;
  type: string;
  purpose?: string;
}

interface PWARelatedApplication {
  platform?: 'chrome_web_store' | 'play' | 'windows' | 'itunes';
  url?: string;
  id?: string;
}

interface PWAShortcut {
  name: string;
  short_name?: string;
  description?: string;
  url: string;
  icons?: PWAIcon[];
}

export interface PWAManifest {
  background_color?: string;
  categories?: string[];
  description?: string;
  dir?: 'auto' | 'ltr' | 'rtl';
  display?: 'fullscreen' | 'standalone' | 'minimal-ui' | 'browser';
  iarc_rating_id?: string;
  icons?: PWAIcon[];
  lang?: string;
  name: string;
  orientation?: 'any' | 'natural' | 'landscape' | 'landscape-primary' | 'landscape-secondary' | 'portrait' | 'portrait-primary' | 'portrait-secondary';
  prefer_related_applications?: boolean;
  related_applications?: PWARelatedApplication[];
  scope?: string;
  screenshots?: PWAIcon[]; 
  short_name?: string;
  shortcuts?: PWAShortcut[];
  start_url?: string;
  theme_color?: string;
}

interface ManifestProps {
  data: PWAManifest;
}


export default class Manifest extends Component<ManifestProps, {}> {
  componentDidMount() {
    const link: HTMLLinkElement | null = document.querySelector('link[rel=manifest]');
    if (!link) return;
    link.href = manifestURL(this.props.data);
  }

  render() {

    // Array.from(document.querySelectorAll('link[rel=manifest]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));
    Array.from(document.querySelectorAll('link[rel="apple-touch-icon"]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));
    Array.from(document.querySelectorAll('meta[name="apple-mobile-web-app-status-bar-style"]:not([data-react-helmet])')).forEach(el => el.parentNode.removeChild(el));

    const { data } = this.props;
    return null;
  }
}