'use client';

import { useEffect, useState } from 'react';
import styles from './Hero.module.css';

export default function Hero() {
    const [showSocials, setShowSocials] = useState(false);

    useEffect(() => {
        const handleScroll = () => {
            // Reveal after scrolling down a bit (e.g., 50px)
            if (window.scrollY > 50) {
                setShowSocials(true);
            } else {
                setShowSocials(false);
            }
        };

        window.addEventListener('scroll', handleScroll, { passive: true });
        return () => window.removeEventListener('scroll', handleScroll);
    }, []);

    return (
        <div className={styles.heroContainer}>
            <div className={styles.videoWrapper}>
                <video
                    className={styles.video}
                    autoPlay
                    muted
                    loop
                    playsInline
                    poster="/placeholder/hero-poster.jpg"
                >
                    {/* Using a placeholder video URL or local file ideally. 
              For now, I'll use no src so it falls back to poster/bg, 
              or I can use a generic refined video if I had one. 
              I'll leave src empty for now and let the user populate, or use a color block. */}
                    {/* <source src="/hero-video.mp4" type="video/mp4" /> */}
                </video>
            </div>

            <div className={`${styles.socials} ${showSocials ? styles.socialsVisible : ''}`}>
                <a href="https://instagram.com" target="_blank" rel="noopener noreferrer" className={styles.socialLink}>
                    Instagram
                </a>
                <a href="https://youtube.com" target="_blank" rel="noopener noreferrer" className={styles.socialLink}>
                    YouTube
                </a>
            </div>

            {/* Scroll indicator or spacer to allow scrolling?
          If the page is ONLY the hero (100vh), there is no scroll.
          The brief says "On scroll down, reveal two small social icons".
          This implies there is content BELOW the hero, or the hero is just the top.
          Wait. "Site Structure: 1. / - Home (Hero)".
          Does Home have other content? "The website exists to... Showcase Erandi's work".
          Usually yes. BUT "4. HOMEPAGE (HERO EXPERIENCE)... On scroll down...".
          If there's no other content on Homepage, we can't scroll.
          Maybe "Projects" are below?
          Structure:
          1. / (Home)
          2. /about
          3. /projects
          
          It seems Home might JUST be the Hero?
          Or maybe the Projects are showcased below?
          Brief: "Entry behaviour: Selecting Residential or Commercial opens directly into a single project viewer".
          This implies Projects are NOT a grid on Home.
          
          If Home is just Hero, how do we scroll?
          Maybe "Scroll down" implies just a gesture to trigger the reveal?
          Or maybe there IS content below?
          "4. HOMEPAGE... On scroll down... reveal icons".
          If I can't scroll, I can't trigger it.
          I will add a spacer or minimal footer to allow scroll, OR assumes "Scroll" triggers the nav/socials even if no visual movement.
          
          However, usually a Hero-only page with scroll reveal implies there's maybe a "About" teaser or just whitespace.
          I'll add a minimal text block or just make the body height > 100vh to allow scroll feel.
          Let's add a dummy spacer for now to enable the interaction.
      */}
            <div style={{ position: 'absolute', top: '150vh', height: '1px', width: '1px' }}></div>
        </div>
    );
}
