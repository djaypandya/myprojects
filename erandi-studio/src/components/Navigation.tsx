'use client';

import Link from 'next/link';
import styles from './Navigation.module.css';

export default function Navigation() {
    return (
        <nav className={styles.nav}>
            <Link href="/" className={styles.logo}>
                Erandi
            </Link>

            <ul className={styles.menu}>
                <li className={styles.menuItem}>
                    <Link href="/about">About</Link>
                </li>
                <li className={styles.menuItem}>
                    <span style={{ cursor: 'default' }}>Projects</span>
                    <div className={styles.dropdown}>
                        <Link href="/projects/residential" className={styles.dropdownLink}>Residential</Link>
                        <Link href="/projects/commercial" className={styles.dropdownLink}>Commercial</Link>
                    </div>
                </li>
                <li className={styles.menuItem}>
                    <Link href="/bespoke">Bespoke</Link>
                </li>
                <li className={styles.menuItem}>
                    <Link href="/contact">Contact</Link>
                </li>
            </ul>
        </nav>
    );
}
